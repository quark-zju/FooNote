use crate::backend::meta::manifest::ManifestBasedBackend;
use crate::backend::meta::manifest::TextIO;
use crate::manifest::min_next_id;
use crate::manifest::Manifest;
use crate::merge;
use crate::t;
use ::sha1::{Digest, Sha1};
use git_cmd::GitCommand;
use notebackend_types::log;
use notebackend_types::Id;
use notebackend_types::PersistCallbackFunc;
use parking_lot::lock_api::RwLockUpgradableReadGuard;
use parking_lot::Mutex;
use parking_lot::RwLock;
use std::borrow::Cow;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Child;
use std::process::Stdio;
use std::sync::Arc;
use std::thread;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;
use tempfile::NamedTempFile;

// Repo structure:
// - notes/{id prefix}/{id suffix} (raw content of note text)
// - manifest.json: {children: {id: [id]}, meta: {id: str}} (metadata)
//
// For a url (no matter local or remote), operate the repo in a local cache
// directory, and only use push and fetch to interact with the url.

/// Git backend using the system git binary.
pub type GitBackend = ManifestBasedBackend<GitTextIO>;

pub struct GitTextIO {
    info: GitInfo,
    // value of texts: Some((content, modified)) | None (deleted)
    texts: RwLock<HashMap<Id, Option<(String, bool)>>>,
    // for "changed" detection.
    last_manifest: Arc<Mutex<Manifest>>,
    // Threads for persist_async() to join.
    persist_threads: Vec<thread::JoinHandle<()>>,
}

/// Information about the Git repository.
/// Can be used to perform "push" without locking "GitTextIO".
#[derive(Clone)]
struct GitInfo {
    remote_name: String,
    repo_path: PathBuf,
    branch_name: String,

    /// Local HEAD. None if there is no local change.
    /// Update on commit.
    local_head_oid: Arc<RwLock<Option<HexOid>>>,

    /// Remote HEAD. Commit hash of the remote branch.b
    /// Update on push and fetch.
    remote_head_oid: Arc<RwLock<Option<HexOid>>>,

    object_reader: Arc<Mutex<Option<Child>>>,
}

const MANIFEST_NAME: &str = "manifest.json";

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct HexOid(String);

impl HexOid {
    fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for HexOid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl TextIO for GitTextIO {
    fn get_raw_text<'a>(&'a self, id: Id) -> io::Result<std::borrow::Cow<'a, str>> {
        let text = self.texts.upgradable_read();
        match text.get(&id) {
            Some(Some((data, _modified))) => Ok(data.clone().into()),
            Some(None) => {
                // Was deleted.
                Ok("".into())
            }
            None => {
                let mut text = RwLockUpgradableReadGuard::upgrade(text);
                let data = self.read_text_from_git(id)?;
                text.insert(id, Some((data.clone(), false)));
                Ok(data.into())
            }
        }
    }

    fn set_raw_text(&mut self, id: Id, text: String) -> io::Result<()> {
        let mut texts = self.texts.write();
        texts.insert(id, Some((text, true)));
        Ok(())
    }

    fn remove_raw_text(&mut self, id: Id) -> io::Result<()> {
        let mut texts = self.texts.write();
        texts.entry(id).and_modify(|e| *e = None);
        Ok(())
    }

    fn persist_with_manifest(&mut self, manifest: &mut Manifest) -> io::Result<()> {
        if self.commit_with_manifest(manifest)?.is_some() || self.info.has_local_changes() {
            self.texts.write().clear();
            self.info.push()?;
        }
        Ok(())
    }

    fn persist_async_with_manifest(
        &mut self,
        manifest: &mut Manifest,
        callback: PersistCallbackFunc,
    ) {
        let sync_result = (|| -> io::Result<Option<HexOid>> {
            let base_commit = self.commit_with_manifest(manifest)?;
            self.texts.write().clear();
            Ok(base_commit)
        })();
        match (sync_result, self.info.has_local_changes()) {
            (Err(e), _) => {
                // Error happened committing.
                callback(Err(e));
            }
            (Ok(None), false) => {
                // Nothing changed.
                callback(Ok(()));
            }
            _ => {
                // Need push.
                let info = self.info.clone();
                let previous_threads: Vec<_> = self.persist_threads.drain(..).collect();
                let handler = thread::spawn(move || {
                    let async_result = info.push();
                    callback(async_result);
                    // Clean up previous threads.
                    for t in previous_threads {
                        let _ = t.join();
                    }
                });
                self.persist_threads.push(handler);
            }
        }
    }
}

impl GitBackend {
    /// Creates a Git backend. `url` specifies the repo location.
    /// `cache_dir` can be None, which means using the system default cache
    /// location, or a specified location for testing purpose.
    pub fn from_git_url(url: &str, cache_dir: Option<&Path>) -> io::Result<GitBackend> {
        let (url, branch_in_url) = split_url(url)?;
        let url = canonicalize_url(url);
        let repo_path = prepare_bare_staging_repo(cache_dir)?;
        maybe_create_local_repo(&Path::new(&url))?;
        let remote_name = prepare_remote_name(&repo_path, &url)?;
        let branch_name = branch_in_url.unwrap_or("").to_string();
        let mut text_io = GitTextIO {
            info: GitInfo::new(repo_path, remote_name, branch_name),
            texts: Default::default(),
            last_manifest: Default::default(),
            persist_threads: Default::default(),
        };
        match text_io.info.fetch() {
            Err(e) => {
                log::warn!("cannot fetch remote: {}", e);
                text_io.info.reload_remote_branch_oid()?;
            }
            Ok(info) => text_io.info = info,
        }
        text_io.info.read_local_head_oid()?;
        let manifest = text_io.load_manifest()?;
        Ok(Self::from_manifest_text_io(manifest, text_io))
    }
}

/// Convert local path to an absolute path.
fn canonicalize_url(url: &str) -> String {
    let path = Path::new(url);
    if url.contains(":") || url.contains("@") || path.is_absolute() {
        url.to_string()
    } else {
        let path = crate::url::APP_DIR.join(path);
        let result = path.display().to_string();
        log::debug!("url {} => {}", &url, &result);
        result
    }
}

fn maybe_create_local_repo(repo_path: &Path) -> io::Result<()> {
    if let Some(parent) = repo_path.parent() {
        if parent.exists() && !repo_path.exists() {
            let path_str = repo_path.display().to_string();
            let args = vec![
                "-c",
                "init.defaultBranch=master",
                "init",
                "--bare",
                &path_str,
            ];
            log::info!("Creating local git repo at {}", path_str);
            GitCommand::default().args(&args).run()?;
        }
    }
    Ok(())
}

impl GitInfo {
    fn new(repo_path: PathBuf, remote_name: String, branch_name: String) -> Self {
        let info = Self {
            remote_name,
            repo_path,
            branch_name,
            local_head_oid: Arc::new(RwLock::new(None)),
            remote_head_oid: Arc::new(RwLock::new(None)),
            object_reader: Default::default(),
        };
        info
    }

    fn local_ref_name(&self) -> String {
        format!("local-{}-{}", self.remote_name, self.branch_name)
    }

    // Update `remote_head_oid`.
    fn reload_remote_branch_oid(&self) -> io::Result<()> {
        *self.remote_head_oid.write() = Some(self.rev_parse(&self.remote_branch_ref_name())?);
        Ok(())
    }

    fn has_local_changes(&self) -> bool {
        let local_head = self.local_head_oid.read();
        if local_head.is_none() {
            // There is no local commit yet.
            false
        } else {
            &*local_head != &*self.remote_head_oid.read()
        }
    }

    /// Create an empty branch remotely.
    /// Useful to support "new empty repo" case with less special cases.
    fn create_empty_branch(&self, branch_name: &str) -> io::Result<()> {
        log::info!("Creating empty branch {}", branch_name);

        // git mktree </dev/null
        const EMPTY_TREE: &str = "4b825dc642cb6eb9a060e54bf8d69288fbee4904";

        // out is the sha1 of the commit.
        let out = self
            .git()
            .args(&["commit-tree", "-m", "[FooNote] Initial branch", EMPTY_TREE])
            .output()?;
        let out = out.trim();

        // Pus to the desired remote branch.
        let push_arg = format!("{}:refs/heads/{}", out, branch_name);
        self.git()
            .args(&["push", self.remote_name.as_str(), push_arg.as_str()])
            .run()?;

        Ok(())
    }

    /// Fetch the latest commit from the remote.
    /// Create a branch if remote repo is empty. Return a `GitInfo` with
    /// the branch name filled.
    /// `self.remote_head_oid will be set to the fetched commit.
    fn fetch(&self) -> io::Result<Self> {
        let mut info = self.clone();
        let args = if info.branch_name.is_empty() {
            // Fetch all branches and pick a branch name.
            self.run_git(&["fetch", self.remote_name.as_str()])?;
            log::info!("Fetched {}", self.remote_name.as_str());
            let pattern = format!("refs/remotes/{}", &self.remote_name);
            let ref_list = self
                .git()
                .args(&[
                    "for-each-ref",
                    "--format=%(refname:lstrip=3)", // strip "refs/remtes/$remote" prefix
                    pattern.as_str(),
                ])
                .output()?;
            let ref_names = ref_list.lines().collect::<BTreeSet<&str>>();
            let branch_name = if ref_names.contains("main") {
                "main"
            } else if ref_names.contains("master") {
                "master"
            } else if let Some(name) = ref_names.iter().next() {
                *name
            } else {
                // An empty repo. Attempt to create a branch.
                let name = "master";
                self.create_empty_branch(name)?;
                name
            };
            log::info!(
                "Implicit remote branch: {} (picked from {:?})",
                branch_name,
                &ref_names
            );
            info.branch_name = branch_name.to_string();
        } else {
            // Fetch specified branch name.
            self.run_git(&[
                "fetch",
                self.remote_name.as_str(),
                self.branch_name.as_str(),
            ])?;
            log::info!(
                "Fetched {}/{}",
                self.remote_name.as_str(),
                self.branch_name.as_str()
            );
        };
        info.reload_remote_branch_oid()?;
        Ok(info)
    }

    /// Push the local changes to remote.
    fn push(&self) -> io::Result<()> {
        if !self.has_local_changes() {
            log::debug!("no local change to push");
            return Ok(());
        }

        let to_push = match self.local_head_oid.read().clone() {
            Some(h) => h,
            None => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    t!(
                        cn = "无法确定用于推送的本地分支",
                        en = "no known local head to push"
                    ),
                ))
            }
        };
        let branch = self.branch_name.as_str();
        let remote = self.remote_name.as_str();
        let (output, error) = self
            .git()
            .args(&[
                "push",
                "--porcelain",
                remote,
                &format!("{}:refs/heads/{}", &to_push, branch,),
            ])
            .output_and_error()?;

        if let Some(error) = error {
            if output.lines().any(|l| l.trim().starts_with("!")) {
                // Try to handle "conflict" (non-fast-forward push) error by merging.
                log::warn!("Push {} to {}/{} was rejected", to_push, remote, branch,);

                // Fetch.
                self.fetch()?;

                // Manual merge.
                let local = to_push;
                let other = match self.remote_head_oid.read().clone() {
                    Some(h) => h,
                    None => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            t!(
                                cn = "无法确定用于合并的远端分支",
                                en = "no known remote head to merge"
                            ),
                        ))
                    }
                };
                let merge = self.merge_conflict(&local, &other)?;

                // Push again.
                self.git()
                    .args(&["push", remote, &format!("{}:refs/heads/{}", &merge.0, &branch)])
                    .run()?;
                log::info!("Pushed merge {} to {}/{}", &merge.0, &remote, &branch);
                self.reload_remote_branch_oid()?;
                Ok(())
            } else {
                // Other errors. Cannot handle.
                let message = format!("Push {} to {}/{} was unsuccessful", to_push, remote, branch);
                log::warn!("{}", &message);
                Err(error)
            }
        } else {
            log::info!("Pushed {} to {}/{}", to_push, remote, branch);
            self.reload_remote_branch_oid()?;
            Ok(())
        }
    }

    /// Create a merge commit that resolves conflicts.
    /// local and other are <commit-ish>.
    /// Will call "commit" and update local_head_oid.
    fn merge_conflict(&self, local_oid: &HexOid, other_oid: &HexOid) -> io::Result<HexOid> {
        let base_oid = HexOid(
            self.git()
                .args(&["merge-base", local_oid.as_str(), other_oid.as_str()])
                .output()?
                .trim()
                .to_string(),
        );
        log::info!(
            "Merging local: {} other: {} base: {}",
            local_oid,
            other_oid,
            &base_oid,
        );

        // Find the difference (file paths) between local and other.
        let diff_name_only = self
            .git()
            .args(&[
                "diff",
                "--name-only",
                "-z",
                local_oid.as_str(),
                other_oid.as_str(),
            ])
            .output()?;
        let paths: Vec<&str> = diff_name_only.split('\0').collect();

        // Read manifest.
        let read_manifest = |commit: &HexOid| -> io::Result<Manifest> {
            let manifest_str = self.read_path_utf8_on_commit(commit, MANIFEST_NAME)?.1;
            serde_json::from_str(&manifest_str)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
        };
        let mut local_manifest: Manifest = read_manifest(&local_oid)?;
        let other_manifest: Manifest = read_manifest(&other_oid)?;
        let mut next_id = local_manifest.next_id.max(other_manifest.next_id);
        let mut local_manifest_id_remap = HashMap::new();

        // Merge the files.
        let mut files = Vec::with_capacity(paths.len() + 1);

        for path in paths {
            let path = path.trim();
            if path == MANIFEST_NAME || path.is_empty() {
                // Manifest is handled later to maintain valid JSON.
                continue;
            }
            let local_data = self.try_read_path_utf8_on_commit(&local_oid, path)?;
            let other_data = self.try_read_path_utf8_on_commit(&other_oid, path)?;
            let base_data = self.try_read_path_utf8_on_commit(&base_oid, path)?;

            // Print debug message.
            let describe = if text_id(path).is_some() {
                |s: &Option<String>| match s.as_ref() {
                    Some(s) => first_line(&s).to_string(),
                    None => "(missing)".to_string(),
                }
            } else {
                |s: &Option<String>| match s.as_ref() {
                    Some(s) => format!("({}b)", s.len()),
                    None => "(missing)".to_string(),
                }
            };
            log::debug!(
                "Merging path {}: local {}, other {}, base {}",
                path,
                describe(&local_data),
                describe(&other_data),
                describe(&base_data),
            );

            match (base_data, local_data, other_data) {
                (_, None, None) => {}
                (_, None, Some(data)) => {
                    log::debug!(" take other (only in other)");
                    // No need to set the file content - the "from" commit is "other".
                    // files.push((path.to_string(), Some(data.into())));
                }
                (_, Some(data), None) => {
                    log::debug!(" take local (only in local)");
                    files.push((path.to_string(), Some(data.into())))
                }
                (Some(base), Some(a), Some(b)) => {
                    let merged = merge::merge_two_strings_with_base(&base, &a, &b);
                    log::debug!(" merge ({}b, {}b) => {}b", a.len(), b.len(), merged.len());
                    files.push((path.to_string(), Some(merged.into())));
                }
                (None, Some(a), Some(b)) => {
                    if let Some(id) = text_id(path) {
                        // Same id but should probably use different id!
                        // Keep path => b unchanged. Use a new path for local (a).
                        let new_id = next_id;
                        let new_path = text_path(new_id);
                        next_id += 1;
                        log::debug!(" rename local (not in base): {} {}", new_id, &new_path);
                        local_manifest_id_remap.insert(id, new_id);
                        files.push((new_path, Some(a.into())));
                    } else {
                        // The path is not a note. Just take the latest remote (other).
                        log::debug!(" take other (not in base, not a note)");
                    }
                }
            }
        }

        // Merge manifest.
        local_manifest.remap_ids(&local_manifest_id_remap);
        local_manifest.merge(&other_manifest);
        let resolved_manifest_str =
            serde_json::to_string_pretty(&local_manifest).expect("manifest has valid JSON format");
        files.push((
            MANIFEST_NAME.to_string(),
            Some(resolved_manifest_str.as_str().into()),
        ));
        let mut parents = vec![other_oid.clone(), local_oid.clone()];
        parents.dedup();
        self.commit(&files, "[FooNote] Merge conflicts", Some(parents))
    }

    fn git(&self) -> GitCommand {
        GitCommand::at(&self.repo_path)
    }

    fn run_git(&self, args: &[&str]) -> io::Result<()> {
        let _out = self.git().args(args).output()?;
        Ok(())
    }

    /// Commit changes on base_commit. Update base_commit. Return the commit hash (hex).
    fn commit<'a>(
        &self,
        files: &'a [(String, Option<Cow<'a, str>>)],
        message: &str,
        parent_commits: Option<Vec<HexOid>>,
    ) -> io::Result<HexOid> {
        let parent_commits =
            parent_commits.unwrap_or_else(|| match self.local_head_oid.read().clone() {
                Some(h) => vec![h],
                None => match self.remote_head_oid.read().clone() {
                    Some(h) => vec![h],
                    None => vec![],
                },
            });
        let payload = fast_import_payload(self, files, message, &parent_commits);
        let mut tmp_file = NamedTempFile::new_in(&self.repo_path)?;
        tmp_file.write_all(&payload.as_bytes())?;
        tmp_file.flush()?;
        let stdin_file = File::open(tmp_file.path())?;

        let args = &["fast-import", "--quiet", "--force"];
        let mut process = self
            .git()
            .stdin(Stdio::from(stdin_file))
            .args(args)
            .spawn()?;

        let stdout = process.stdout.as_mut().expect("piped stdin");
        let mut commit_oid = String::new();
        stdout.read_to_string(&mut commit_oid)?;

        let status = process.wait()?;
        if !status.success() {
            let code = status.code().unwrap_or_default();
            log::warn!("fastimport failed (exit code: {})", code);
            return Err(io::Error::new(
                io::ErrorKind::Other,
                t!(
                    cn = "git fastimport 失败 (返回值: {})",
                    en = "git fastimport failed (exit code: {})",
                    code
                ),
            ));
        }
        drop(tmp_file);

        let commit_oid = HexOid(commit_oid.trim().to_string());
        if commit_oid.0.is_empty() {
            return notebackend_types::error::invalid_input(t!(
                cn = "git fastimport 未提供新的提交的 SHA 值",
                en = "git fastimport did not provide new commit hash"
            ));
        }
        log::info!("Committed: {}", &commit_oid.0);
        *self.local_head_oid.write() = Some(commit_oid.clone());
        Ok(commit_oid)
    }

    /// Read local_head_oid from disk.
    /// The local_head_oid is discarded if it can fast-forward to remote oid.
    fn read_local_head_oid(&self) -> io::Result<()> {
        let local_ref_name = self.local_ref_name();
        let mut local_head_oid = self.local_head_oid.write();
        if local_head_oid.is_none() {
            if let Ok(local_oid) = self.rev_parse(&local_ref_name) {
                // Check if local_oid is ahead of remote_oid (unsaved changes).
                if let Some(remote_oid) = self.remote_head_oid.read().clone() {
                    let merge_oid = self
                        .git()
                        .args(&["merge-base", local_oid.as_str(), remote_oid.as_str()])
                        .output()?;
                    let merge_oid = HexOid(merge_oid.trim().to_string());
                    if merge_oid == local_oid {
                        log::debug!(
                            "local {} can fast-forward to remote {}",
                            local_oid.as_str(),
                            remote_oid.as_str()
                        );
                    } else if merge_oid == remote_oid {
                        log::info!(
                            "local {} is ahead of remote {}",
                            local_oid.as_str(),
                            remote_oid.as_str()
                        );
                        *local_head_oid = Some(local_oid);
                    } else {
                        log::debug!(
                            "local {} and remote {} need a merge",
                            local_oid.as_str(),
                            remote_oid.as_str()
                        );
                        drop(local_head_oid);
                        self.merge_conflict(&local_oid, &remote_oid)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Read an object (git-cat). Return (oid, data). There is no caching.
    /// `spec` is like `commit:path`, or `oid`. See `git cat-file --batch`
    /// man page.
    /// If `expected_type` is set (ex. `blob`), raise an error if type
    /// mismatches.
    fn read_object(
        &self,
        spec: &str,
        expected_type: Option<&str>,
    ) -> io::Result<(HexOid, Vec<u8>)> {
        let mut object_reader = self.object_reader.lock();
        if object_reader.is_none() {
            let child = self
                .git()
                .stdin(Stdio::piped())
                .args(&["cat-file", "--batch"])
                .spawn()?;
            *object_reader = Some(child);
        }
        let reader = {
            let reader = object_reader.as_mut().expect("child created above");
            if let Ok(Some(_)) = reader.try_wait() {
                log::info!("git cat-file has exited - restarting");
                let child = self
                    .git()
                    .stdin(Stdio::piped())
                    .args(&["cat-file", "--batch"])
                    .spawn()?;
                *object_reader = Some(child);
                object_reader.as_mut().expect("child created above")
            } else {
                reader
            }
        };
        let stdin = reader
            .stdin
            .as_mut()
            .expect("child created with piped stdin");
        stdin.write_all(format!("{}\n", spec).as_bytes())?;
        stdin.flush()?;
        let stdout = reader
            .stdout
            .as_mut()
            .expect("child crated with piped stdout");
        let first_line = {
            // oid type len_str
            let mut buf = Vec::with_capacity(64);
            loop {
                let mut b = [0];
                stdout.read_exact(&mut b)?;
                if b[0] == b'\n' {
                    break;
                }
                buf.push(b[0]);
            }
            String::from_utf8(buf).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?
        };
        if first_line.contains("missing") {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                t!(
                    cn = "Git 对象 {} 未找到",
                    en = "git object {} is not found",
                    spec
                ),
            ));
        }
        if first_line.contains("ambiguous") {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                t!(
                    cn = "Git 对象 {} 有歧义",
                    en = "git object {} is ambiguous",
                    spec
                ),
            ));
        }
        let (oid, obj_type, len) = {
            let split: Vec<&str> = first_line.split(" ").collect();
            if split.len() != 3 || split[0].len() != 40 || split[2].parse::<usize>().is_err() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    t!(
                        cn = "git cat-file {} 返回无效信息: {:?}",
                        en = "git cat-file {} returned malformed header: {:?}",
                        spec,
                        first_line
                    ),
                ));
            }
            (
                split[0],
                split[1],
                split[2].parse::<usize>().expect("parsed above"),
            )
        };
        if let Some(expected_type) = expected_type {
            if expected_type != obj_type {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    t!(
                        cn = "Git 对象 {} 类型不匹配：{} != {}",
                        en = "git object {} has type {}, but expect {}",
                        spec,
                        obj_type,
                        expected_type
                    ),
                ));
            }
        }
        let mut data = vec![0u8; len + 1];
        stdout.read_exact(&mut data)?;
        if data.last() != Some(&b'\n') {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                t!(
                    cn = "git cat-file {} 返回无效末尾",
                    en = "git cat-file {} returned malformed end",
                    spec
                ),
            ));
        }
        data.pop(); // Remove the last LF.
        Ok((HexOid(oid.to_string()), data))
    }

    /// Read a utf-8 file on `base_commit`. Return (oid, text).
    fn read_path_utf8_on_base(&self, file_path: &str) -> io::Result<(HexOid, String)> {
        self.read_path_utf8_on_commit(&self.head_oid()?, file_path)
    }

    /// Read a utf-8 file on the given commit. Return (oid, text).
    fn read_path_utf8_on_commit(
        &self,
        commit: &HexOid,
        file_path: &str,
    ) -> io::Result<(HexOid, String)> {
        let spec = format!("{}:{}", commit, file_path);
        let (oid, data) = self.read_object(&spec, Some("blob"))?;
        Ok((oid, String::from_utf8_lossy(&data).into_owned()))
    }

    /// Read a utf-8 file on the given commit. Do not fail if the file is missing.
    /// Return None if the file is missin. Or the file content.
    fn try_read_path_utf8_on_commit(
        &self,
        commit: &HexOid,
        file_path: &str,
    ) -> io::Result<Option<String>> {
        let spec = format!("{}:{}", commit, file_path);
        match self.read_object(&spec, Some("blob")) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e),
            Ok((_oid, data)) => Ok(Some(String::from_utf8_lossy(&data).to_string())),
        }
    }

    /// ex. "remote/branch", ref name of the remote ref.
    fn remote_branch_ref_name(&self) -> String {
        format!("{}/{}", &self.remote_name, &self.branch_name)
    }

    /// Converts a ref name to a commit hash.
    fn rev_parse(&self, commitish: &str) -> io::Result<HexOid> {
        let s = self
            .git()
            .args(&["rev-parse", commitish])
            .output()?
            .trim()
            .to_string();
        Ok(HexOid(s))
    }

    /// Local committed oid, or remote oid.
    fn head_oid(&self) -> io::Result<HexOid> {
        match self.local_head_oid.read().clone() {
            Some(h) => Ok(h),
            None => match self.remote_head_oid.read().clone() {
                Some(h) => Ok(h),
                None => notebackend_types::error::invalid_data(t!(
                    cn = "未知本地或远程分支",
                    en = "neither local or remote head is known"
                )),
            },
        }
    }
}

/// Path for the text object.
fn text_path(id: Id) -> String {
    format!("notes/{:x}/{:x}", id / 256, id % 256)
}

/// Reverse of `text_path`.
fn text_id(path: &str) -> Option<Id> {
    let path = path.replace('\\', "/");
    let split = path.split('/').collect::<Vec<_>>();
    if let ["notes", prefix, reminder] = split[..] {
        let prefix = u32::from_str_radix(prefix, 16).ok()?;
        let reminder = u32::from_str_radix(reminder, 16).ok()?;
        return Some((prefix * 256 + reminder) as _);
    }
    None
}

impl GitTextIO {
    fn read_text_from_git(&self, id: Id) -> io::Result<String> {
        let path = text_path(id);
        let data = match self.info.read_path_utf8_on_base(&path) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => String::new(),
            Ok((_oid, data)) => data,
            Err(e) => return Err(e),
        };
        Ok(data)
    }

    /// Load the metadata (parents, children, meta).
    /// Updates `last_manifest` state.
    fn load_manifest(&mut self) -> io::Result<Manifest> {
        let (oid, manifest_str) = match self.info.read_path_utf8_on_base(MANIFEST_NAME) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => (None, "{}".to_string()),
            Ok((oid, s)) => (Some(oid), s),
            Err(e) => return Err(e),
        };
        let mut manifest: Manifest = serde_json::from_str(&manifest_str)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        manifest.next_id = manifest.next_id.max(min_next_id());
        manifest.rebuild_parents();
        *self.last_manifest.lock() = manifest.clone();
        Ok(manifest)
    }

    /// Mark text that has changed back to original state as "unchanged".
    /// Return the count of changed "text"s.
    fn optimize_then_count_changed_files(&mut self, manifest: &mut Manifest) -> io::Result<usize> {
        let mut changed_count = 0;
        let mut texts = self.texts.write();

        // Clean up unreachable.
        let unreachable = manifest.remove_unreachable();
        for id in unreachable {
            // Mark as deletion.
            texts.insert(id, None);
        }

        // Go through texts changes.
        for (&id, opt) in texts.iter_mut() {
            if let Some((text, changed)) = opt {
                if *changed {
                    let orig_text = self.read_text_from_git(id)?;
                    if &orig_text == text {
                        log::trace!("Text not changed: {}", id);
                        *changed = false;
                    } else {
                        log::trace!("Text changed: {}", id);
                        changed_count += 1;
                    }
                }
            }
        }
        log::debug!("Changed text count: {}", changed_count);
        Ok(changed_count)
    }

    /// Commit changes. Return the hex commit hash.
    /// Update self.base_commit to the new commit.
    /// Return None if nothing has changed and there is no need to commit.
    fn commit_with_manifest(&mut self, manifest: &mut Manifest) -> io::Result<Option<HexOid>> {
        // Nothing changed?
        if self.optimize_then_count_changed_files(manifest)? == 0 {
            if &*self.last_manifest.lock() == manifest {
                log::info!("Nothing changed - No need to commit");
                return Ok(None);
            } else {
                log::debug!("Manifest has changed");
            }
        }

        let last_manifest = self.last_manifest.clone();
        self.with_changed_files(manifest, |files| {
            let commit_oid = self.info.commit(&files, "[FooNote] Checkpoint", None)?;
            *last_manifest.lock() = manifest.clone();
            Ok(Some(commit_oid))
        })
    }

    /// Calculate "changed files" (path, Some(data) /* write */ | None /* delete */)
    /// for committing.
    fn with_changed_files<R>(
        &self,
        manifest: &Manifest,
        f: impl FnOnce(&[(String, Option<Cow<str>>)]) -> R,
    ) -> R {
        let manifest_str =
            serde_json::to_string_pretty(&manifest).expect("Manifest can be encoded");
        let texts = self.texts.read();
        let files: Vec<(String, Option<Cow<str>>)> =
            std::iter::once((MANIFEST_NAME.to_string(), Some(manifest_str.as_str().into())))
                .chain(texts.iter().filter_map(|(&id, data)| {
                    match data {
                Some((data, true /* modified */ )) => Some((text_path(id), Some(data.as_str().into()))),
                None /* deletion */ => Some((text_path(id), None)),
                Some((data, false /* not modified */ )) => None,
            }
                }))
                .collect();
        f(&files)
    }
}

/// Prepare payload for git fastimport.
fn fast_import_payload<'a>(
    info: &GitInfo,
    // (path, content)
    files: &'a [(String, Option<Cow<'a, str>>)],
    // commit message
    message: &str,
    // parent commits
    parent_commits: &[HexOid],
) -> String {
    // Prepare fast-import payload.
    let mut payload = String::with_capacity(4096);

    // Prepare the blobs.
    for (i, &(ref path, ref text)) in files.iter().enumerate() {
        if let Some(text) = text {
            let blob = fast_import_blob(text.as_ref(), i);
            payload += &blob;
        }
    }

    // Prepare the commit object.
    let local_ref_name = info.local_ref_name();
    let when = format!("{} +0000", epoch());
    let file_modifications = {
        let mut modifications = vec![];
        for (i, &(ref path, ref text)) in files.iter().enumerate() {
            match text {
                Some(text) => {
                    modifications.push(fast_import_file_modify(&path, i));
                }
                None => {
                    modifications.push(fast_import_file_delete(&path));
                }
            }
        }
        modifications
    };

    let commit_id = files.len() + FASTIMPORT_ID_MARK_OFFSET;
    let parents_lines = parent_commits
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let prefix = if i == 0 { "from" } else { "merge" };
            format!("{} {}\n", prefix, s)
        })
        .collect::<Vec<_>>()
        .concat();
    let commit = format!(
        concat!(
            "commit refs/tags/{ref_name}\n",
            "mark :{commit_id}\n",
            "committer {name} <{email}> {when}\n",
            "data {message_len}\n{message}\n",
            "{parents_lines}",
            "{files}",
            "\n",
            "get-mark :{commit_id}\n",
        ),
        ref_name = local_ref_name,
        commit_id = commit_id,
        name = git_cmd::GIT_USER.as_str(),
        email = git_cmd::GIT_EMAIL.as_str(),
        when = when,
        message_len = message.len(),
        message = &message,
        parents_lines = parents_lines,
        files = file_modifications.concat(),
    );
    payload += &commit;

    payload
}

impl Drop for GitTextIO {
    fn drop(&mut self) {
        let mut object_reader = self.info.object_reader.lock();
        if let Some(ref mut child) = *object_reader {
            // Close stdin pipe.
            child.stdin = None;
            // Wait for the process to end.
            let _ = child.wait();
        }
        for t in self.persist_threads.drain(..) {
            let _ = t.join();
        }
    }
}

fn epoch() -> u64 {
    let d = match SystemTime::now().duration_since(UNIX_EPOCH) {
        Err(_) => return 0,
        Ok(d) => d,
    };
    d.as_secs()
}

/// Create a remote pointing to the url on demand. Return the remote name.
fn prepare_remote_name(repo_path: &Path, url: &str) -> io::Result<String> {
    // The name is based on the SHA1 of the `url`.
    let remote_name = format!("rh_{}", sha1_hex(url.as_bytes()));

    let existing_url = GitCommand::at(repo_path)
        .args(&["remote", "get-url", remote_name.as_str()])
        .output()
        .ok();

    if let Some(existing_url) = existing_url {
        // The remote exists. Check its URL.
        let existing_url = existing_url.trim();
        if existing_url != url {
            log::warn!(
                "remote url for {} differs: {} vs {}",
                remote_name,
                existing_url,
                url
            );
        }
    } else {
        // Create the remote name.
        let _out = GitCommand::at(repo_path)
            .args(&["remote", "add", remote_name.as_str(), url])
            .context(t!(
                cn = "创建远端分支 {} ({}, {})",
                en = "create remote {} for url {} repo at {}",
                remote_name,
                url,
                repo_path.display()
            ))
            .output()?;
    }

    Ok(remote_name.to_string())
}

/// Split git:foo/bar.git#main to (git:foo/bar, main).
fn split_url(url: &str) -> io::Result<(&str, Option<&str>)> {
    let split: Vec<&str> = url.rsplitn(2, '#').collect();
    match split[..] {
        [url] => Ok((url, None)),
        [hash, url] => Ok((url, Some(hash))),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                t!(cn = "无效地址: {}", en = "invalid url: {}", url),
            ))
        }
    }
}

fn default_next_id() -> Id {
    10
}

/// Make sure a "bare" repo exists in a local cache directory.
fn prepare_bare_staging_repo(cache_dir: Option<&Path>) -> io::Result<PathBuf> {
    let cache_dir = match cache_dir {
        None => match dirs::cache_dir() {
            Some(d) => d,
            None => std::env::current_dir()?,
        },
        Some(d) => d.to_path_buf(),
    };
    let path = cache_dir.join("foonote").join("git");
    if path.join("config").exists() {
        return Ok(path);
    }
    std::fs::create_dir_all(&path)?;
    let path_str = path.display().to_string();
    GitCommand::default()
        .args(&["init", "--bare", path_str.as_str()])
        .run()?;
    log::info!("staging repo: {}", path.display());
    Ok(path)
}

const FASTIMPORT_ID_MARK_OFFSET: usize = 1;

fn fast_import_blob(text: &str, id: usize) -> String {
    format!(
        "blob\nmark :{}\ndata {}\n{}\n",
        id + FASTIMPORT_ID_MARK_OFFSET,
        text.len(),
        text
    )
}

fn fast_import_file_modify(path: &str, id: usize) -> String {
    format!("M 100644 :{} {}\n", id + FASTIMPORT_ID_MARK_OFFSET, path)
}

fn fast_import_file_delete(path: &str) -> String {
    format!("D {}\n", path)
}

mod git_cmd {
    use notebackend_types::log;
    use once_cell::sync::Lazy;
    use std::io;
    use std::path::Path;
    use std::path::PathBuf;
    use std::process::Child;
    use std::process::Command;
    use std::process::Stdio;

    static GIT: &str = "git";
    pub(crate) static GIT_USER: Lazy<String> = Lazy::new(git_config_user);
    pub(crate) static GIT_EMAIL: Lazy<String> = Lazy::new(git_config_email);

    /// Wrapper of `Command` with easier logging / debugging.
    pub(crate) struct GitCommand {
        git_dir: Option<PathBuf>,
        args: Vec<String>,
        context: Option<String>,
        stdin: Stdio,
    }

    impl Default for GitCommand {
        fn default() -> Self {
            Self {
                git_dir: None,
                args: Default::default(),
                context: None,
                stdin: Stdio::null(),
            }
        }
    }

    impl GitCommand {
        /// At the given directory.
        pub fn at(dir: &Path) -> Self {
            Self {
                git_dir: Some(dir.to_path_buf()),
                args: Default::default(),
                context: None,
                stdin: Stdio::null(),
            }
        }

        /// Specify command line arguments.
        pub fn args(mut self, args: &[&str]) -> Self {
            self.args.extend(args.iter().map(|s| s.to_string()));
            self
        }

        /// Specify stdin.
        pub fn stdin(mut self, stdin: Stdio) -> Self {
            self.stdin = stdin;
            self
        }

        /// Specify error context.
        pub fn context(mut self, context: String) -> Self {
            self.context = Some(context);
            self
        }

        /// Run. Check exit code.
        pub fn run(&mut self) -> io::Result<()> {
            let _out = self.output()?;
            Ok(())
        }

        /// Run the command. Return output. Check exit code.
        pub fn output(&mut self) -> io::Result<String> {
            let (out, err) = self.output_and_error()?;
            if let Some(err) = err {
                return Err(err);
            }
            Ok(out)
        }

        /// Run the command. Return both the output and the error (if exit code is not 0).
        pub fn output_and_error(&mut self) -> io::Result<(String, Option<io::Error>)> {
            let out = match self.command(|c| c.output()) {
                Err(e) => {
                    return Err(self.error(format!("cannot spawn {} {:?} {}", GIT, &self.args, e)));
                }
                Ok(o) => o,
            };
            let stdout = String::from_utf8_lossy(&out.stdout);
            let stderr = String::from_utf8_lossy(&out.stderr);
            let code = out.status.code().unwrap_or_default();
            let mut error = None;
            if !out.status.success() {
                log::warn!("{} {:?} exited {}", GIT, &self.args, code);
                let at = self
                    .git_dir
                    .as_ref()
                    .map(|s| s.display().to_string())
                    .unwrap_or_else(|| ".".to_string());
                error = Some(self.error(format!(
                    "{} {:?} at {} exited {} with output [\n{}][\n{}]",
                    GIT, &self.args, at, code, stdout, stderr,
                )));
            } else {
                log::debug!("{} {:?} completed", GIT, &self.args);
            }

            if !stdout.is_empty() {
                log::debug!("stdout:\n{}", &stdout);
            }
            if !stderr.is_empty() {
                log::debug!("stderr:\n{}", &stderr);
            }

            Ok((stdout.to_string(), error))
        }

        /// Spawn the git process with stdio piped.
        pub fn spawn(&mut self) -> io::Result<Child> {
            self.command(|c| c.stdout(Stdio::piped()).spawn())
                .map_err(|e| self.error(format!("cannot spawn {} {:?} {}", GIT, &self.args, e)))
        }

        /// Prepare the `Command`. Then call `f` on `Command`.
        fn command<T>(&mut self, f: impl FnOnce(&mut Command) -> T) -> T {
            let mut cmd = Command::new(GIT);
            let mut cmd = cmd
                // Do not use GIT_USER here - can deadlock.
                .env("GIT_AUTHOR_NAME", "FooNote")
                .env("GIT_AUTHOR_EMAIL", "<foonote@example.com>")
                .env("GIT_COMMITTER_NAME", "FooNote")
                .env("GIT_COMMITTER_EMAIL", "<foonote@example.com>")
                .stdin({
                    let mut stdin = Stdio::null();
                    std::mem::swap(&mut self.stdin, &mut stdin);
                    stdin
                });
            #[cfg(windows)]
            {
                use std::os::windows::process::CommandExt;
                const CREATE_NO_WINDOW: u32 = 0x08000000;
                cmd = cmd.creation_flags(CREATE_NO_WINDOW);
            }
            if let Some(dir) = self.git_dir.as_ref() {
                log::debug!("running {} {:?} at {}", GIT, &self.args, dir.display());
                let cwd = if dir.file_name().unwrap_or_default() == ".git" {
                    dir.parent().unwrap()
                } else {
                    dir
                };
                cmd = cmd.arg("--git-dir").arg(dir).current_dir(cwd)
            } else {
                log::debug!("running {} {:?}", GIT, &self.args);
            }
            cmd = cmd.args(&self.args);
            f(cmd)
        }

        fn error(&self, mut s: String) -> io::Error {
            if let Some(context) = self.context.as_ref() {
                s = format!("{} - {}", context, s)
            }
            io::Error::new(io::ErrorKind::Other, s)
        }
    }

    fn git_config(key: &str) -> Option<String> {
        let output = GitCommand::default()
            .args(&["config", "--get", key])
            .output()
            .unwrap_or_default()
            .trim()
            .to_string();
        if output.is_empty() {
            // Normalize empty strings to `None` values.
            None
        } else {
            Some(output)
        }
    }

    fn git_config_user() -> String {
        git_config("user.name").unwrap_or_else(|| "FooNote".to_string())
    }

    fn git_config_email() -> String {
        git_config("user.email").unwrap_or_else(|| "foonote@example.com".to_string())
    }
}

/// Extract the first line of message.
fn first_line(s: &str) -> &str {
    s.lines().next().unwrap_or("(untitled)")
}

/// Calculate SHA1 hex of `data`.
fn sha1_hex(data: &[u8]) -> String {
    let mut hasher = Sha1::new();
    hasher.update(data);
    let hash: [u8; 20] = hasher.finalize().into();
    hex::encode(&hash)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::tests::*;
    use crate::clipboard;
    use notebackend_types::TreeBackend;

    fn init_git_repo(dir: &str, bare: bool) -> Option<&str> {
        let mut args = vec!["-c", "init.defaultBranch=m", "init"];
        if bare {
            args.push("--bare");
        }
        args.push(dir);
        match GitCommand::default().args(&args).run().ok() {
            None => {
                eprintln!("skip test - git init does not work properly");
                return None;
            }
            Some(_) => {}
        }
        Some(dir)
    }

    fn populate_git_repo(dir: &Path) -> Option<PathBuf> {
        let git_repo_path = dir.join("repo");
        let git_dir = git_repo_path.join(".git");

        let git_repo_path_str = git_repo_path.display().to_string();
        init_git_repo(&git_repo_path_str, false)?;

        // Make a commit on the master branch.
        std::fs::write(git_repo_path.join("x"), b"x").unwrap();
        let git = |args: &[&str]| GitCommand::at(&git_dir).args(args).run().unwrap();
        git(&["add", "x"]);
        git(&["commit", "-m", "add x"]);
        git(&["checkout", "-b", "mytrunk"]);
        git(&["config", "--add", "receive.denyCurrentBranch", "ignore"]);

        Some(git_repo_path)
    }

    #[test]
    fn test_basic() {
        let dir = tempfile::tempdir().unwrap();
        let cache_path = dir.path().join("cache");
        let git_repo_path = match populate_git_repo(dir.path()) {
            Some(path) => path,
            None => return, /* git does not work */
        };

        let url = format!("{}#mytrunk", git_repo_path.display());
        let mut backend = GitBackend::from_git_url(&url, Some(&cache_path)).unwrap();
        backend.check_generic().unwrap();
        backend.check_generic().unwrap();

        backend.persist().unwrap();
        backend.check_generic().unwrap();
        backend.check_generic().unwrap();

        // Serialize to bytes to test Eq.
        let copied = clipboard::copy(&backend, &[backend.get_root_id()]).unwrap();
        let check = |mut backend: GitBackend| {
            // Check Eq.
            let copied2 = clipboard::copy(&backend, &[backend.get_root_id()]).unwrap();
            assert_eq!(copied, copied2);
            // Check generic operations.
            backend.check_generic().unwrap();
        };

        // Backend should not change after persist().
        backend.persist().unwrap();
        check(backend);

        // Test re-load from the original repo using the same cache.
        let backend = GitBackend::from_git_url(&url, Some(&cache_path)).unwrap();
        check(backend);

        // Test re-load from the original repo using the a different cache.
        let cache_path = dir.path().join("cache2");
        let backend = GitBackend::from_git_url(&url, Some(&cache_path)).unwrap();
        check(backend);
    }

    #[test]
    fn test_conflict_handling() {
        let dir = tempfile::tempdir().unwrap();
        let cache_path = dir.path().join("cache");
        let git_repo_path = match populate_git_repo(dir.path()) {
            Some(path) => path,
            None => return, /* git does not work */
        };

        let url = format!("{}#mytrunk", git_repo_path.display());
        let mut backend1 = GitBackend::from_git_url(&url, Some(&cache_path)).unwrap();
        let ids = backend1.insert_ascii(
            r#"
                  C
                 /
            A---B--II--JJ
             \   \
              D   E
        "#,
        );
        let a = backend1.find("A");
        backend1.update_meta(a, "a=", "b").unwrap();
        backend1.update_meta(a, "c=", "d").unwrap();
        let b = backend1.find("B");
        let i = backend1.find("II");
        let j = backend1.find("JJ");
        backend1.persist().unwrap();

        // "Fork" the current state.
        let mut backend2 = GitBackend::from_git_url(&url, Some(&cache_path)).unwrap();
        backend2.update_meta(a, "a2=", "b2").unwrap();
        backend2.remove(backend2.find("D")).unwrap();
        backend2.quick_insert(b, "F");
        backend2.set_text(a, "A2".to_string()).unwrap();
        backend2.set_text(b, "b\nc\nd\n".to_string()).unwrap();
        backend2.set_text(i, "I".to_string()).unwrap();
        backend2.persist().unwrap();

        // Cause conflict.
        backend1.update_meta(a, "a1=", "b1").unwrap();
        backend1.remove(backend1.find("C")).unwrap();
        backend1.set_text(a, "A1".to_string()).unwrap();
        backend1.set_text(b, "a\nc\nd\ne\n".to_string()).unwrap();
        backend1.set_text(j, "J".to_string()).unwrap();
        backend1
            .set_text(backend1.find("E"), "E1".to_string())
            .unwrap();
        backend1.quick_insert(b, "G");

        // Push. Trigger auto merge.
        backend1.persist().unwrap();

        // Check the merge resolution.
        let backend3 = GitBackend::from_git_url(&url, Some(&cache_path)).unwrap();
        let ids = backend3.all_ids();

        // Text:
        // A => (A1, A2) => A12.
        // B => ("a c d e", "b c d") => "ab c d e"
        // C => (_, C) => C
        // D => (D, _) => D
        // E => (E1, E) => E1
        // _ => (G, F) => (G, F)  # G is re-assigned from 15 to 16.
        //
        // Meta:
        // a=b c=d => (+a2=b2, +a1=b1) => a=b c=d a1=b1 a2=b2

        assert_eq!(
            backend3.draw_ascii_all(),
            r#"
                0 text=""
                \_ 10 text="A12" meta="a=b\nc=d\na1=b1\na2=b2\n"
                   \_ 11 text="ab\nc\nd\ne\n"
                   |  \_ 12 text="C"
                   |  \_ 14 text="E1"
                   |  \_ 15 text="I"
                   |  |  \_ 16 text="J"
                   |  \_ 18 text="G" meta="t=G"
                   |  \_ 17 text="F" meta="t=F"
                   \_ 13 text="D""#
        );
    }

    #[test]
    fn test_push_remote_temporarily_down() {
        let dir = tempfile::tempdir().unwrap();
        let cache_path = dir.path().join("cache");
        let git_repo_path1 = match populate_git_repo(dir.path()) {
            Some(path) => path,
            None => return, /* git does not work */
        };
        let git_repo_path2 = git_repo_path1.with_extension(".tmp");

        // Create changes that are local only (not pushed).
        let url1 = format!("{}#mytrunk", git_repo_path1.display());
        {
            let mut backend1 = GitBackend::from_git_url(&url1, Some(&cache_path)).unwrap();
            backend1.insert_ascii(r#"A"#);
            backend1.persist().unwrap();

            // Make the remote repo temporarily unavailable.
            std::fs::rename(&git_repo_path1, &git_repo_path2).unwrap();
            backend1.insert_ascii("B");
            // The push will fail. But local commit is done for "B".
            backend1.persist().unwrap_err();
        }

        // Check that reloading the URL will pick up the local changes (B).
        // The backend should still work even if `fetch` will fail.
        {
            let backend1 = GitBackend::from_git_url(&url1, Some(&cache_path)).unwrap();
            assert_eq!(
                backend1.draw_ascii_all(),
                r#"
                0 text=""
                \_ 10 text="A"
                \_ 11 text="B""#
            );
        }

        // Change the remote git repo via another backend.
        let url2 = format!("{}#mytrunk", git_repo_path2.display());
        {
            let mut backend2 = GitBackend::from_git_url(&url2, Some(&cache_path)).unwrap();
            backend2.insert_ascii("C");
            backend2.persist().unwrap();
            // Rename back to restore the repo.
            std::fs::remove_dir_all(&git_repo_path1).unwrap();
            std::fs::rename(&git_repo_path2, &git_repo_path1).unwrap();
        }

        // Reload - local changes (B) should be picked up and merged.
        {
            let mut backend1 = GitBackend::from_git_url(&url1, Some(&cache_path)).unwrap();
            assert_eq!(
                backend1.draw_ascii_all(),
                r#"
                0 text=""
                \_ 10 text="A"
                \_ 12 text="B"
                \_ 11 text="C""#
            );
            // And can be pushed.
            backend1.persist().unwrap();
        }

        // Check the merge preserves the changes.
        {
            let backend1 = GitBackend::from_git_url(&url1, Some(&cache_path)).unwrap();
            assert_eq!(
                backend1.draw_ascii_all(),
                r#"
                0 text=""
                \_ 10 text="A"
                \_ 12 text="B"
                \_ 11 text="C""#
            );
        }
    }

    #[test]
    fn test_auto_create_remote_branch() {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();
        let cache_path = dir.join("cache");
        let repo_path_str = dir.join("repo").display().to_string();
        match init_git_repo(&repo_path_str, true) {
            Some(_) => {}
            None => return, /* git does not work */
        }
        let mut backend = GitBackend::from_git_url(&repo_path_str, Some(&cache_path)).unwrap();
        backend.quick_insert(backend.get_root_id(), "a");
        backend.persist().unwrap();
    }

    #[test]
    fn test_auto_create_local_repo() {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.path();
        let cache_path = dir.join("cache");

        let repo_path_str = dir.join("repo1").display().to_string();
        match init_git_repo(&repo_path_str, true) {
            Some(_) => {}
            None => return, /* git does not work */
        }

        // Repo2 will be created automatically.
        let repo_path_str = dir.join("repo2").display().to_string();
        let mut backend = GitBackend::from_git_url(&repo_path_str, Some(&cache_path)).unwrap();
        backend.quick_insert(backend.get_root_id(), "a");
        backend.persist().unwrap();
    }
}
