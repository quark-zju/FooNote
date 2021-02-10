use crate::backend::meta::manifest::ManifestBasedBackend;
use crate::backend::meta::manifest::TextIO;
use crate::manifest::min_next_id;
use crate::manifest::Manifest;
use git_cmd::GitCommand;
use notebackend_types::Id;
use parking_lot::lock_api::RwLockUpgradableReadGuard;
use parking_lot::Mutex;
use parking_lot::RwLock;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Child;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::Mutex as StdMutex;
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
    object_reader: Mutex<Option<Child>>,
    // for "changed" detection.
    last_manifest: Manifest,
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

    /// Commit of the "unmodified" version.
    /// - Commit hash form: Locally committed. Not pushed.
    /// - "remote/branch" form: Pushed. No local changes.
    base_commit: Arc<RwLock<String>>,
}

const MANIFEST_NAME: &str = "manifest.json";

#[derive(Clone, Debug)]
struct HexOid(String);

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
        result: Arc<StdMutex<Option<io::Result<()>>>>,
    ) {
        let sync_result = (|| -> io::Result<Option<String>> {
            let base_commit = self.commit_with_manifest(manifest)?;
            self.texts.write().clear();
            Ok(base_commit)
        })();
        match (sync_result, self.info.has_local_changes()) {
            (Err(e), _) => {
                // Error happened committing.
                *result.lock().unwrap() = Some(Err(e));
            }
            (Ok(None), false) => {
                // Nothing changed.
                *result.lock().unwrap() = Some(Ok(()));
            }
            _ => {
                // Need push.
                let info = self.info.clone();
                let previous_threads: Vec<_> = self.persist_threads.drain(..).collect();
                let handler = thread::spawn(move || {
                    let async_result = info.push();
                    *result.lock().unwrap() = Some(async_result);
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
        let (url, hash) = split_url(url)?;
        let repo_path = prepare_bare_staging_repo(cache_dir)?;
        let remote_name = prepare_remote_name(&repo_path, url)?;
        let branch_name = hash.unwrap_or("master").to_string();
        let mut text_io = GitTextIO {
            info: GitInfo::new(repo_path, remote_name, branch_name),
            texts: Default::default(),
            object_reader: Default::default(),
            last_manifest: Manifest::default(),
            persist_threads: Default::default(),
        };
        text_io.info.fetch()?;
        let manifest = text_io.load_manifest()?;
        Ok(Self::from_manifest_text_io(manifest, text_io))
    }

    /// Enable or disable trash.
    pub fn with_trash(mut self, enabled: bool) -> Self {
        self.manifest = self.manifest.with_trash(enabled);
        self
    }
}

impl GitInfo {
    fn new(repo_path: PathBuf, remote_name: String, branch_name: String) -> Self {
        let info = Self {
            remote_name,
            repo_path,
            branch_name,
            base_commit: Default::default(),
        };
        *info.base_commit.write() = info.base_commit_from_remote();
        info
    }

    /// The "base commit" name in "remote/branch" form.
    fn base_commit_from_remote(&self) -> String {
        format!("{}/{}", &self.remote_name, &self.branch_name)
    }

    fn has_local_changes(&self) -> bool {
        self.base_commit.read().as_str() != self.base_commit_from_remote().as_str()
    }

    /// Fetch the latest commit from the remote.
    /// NOTE: `self.base_commit` is not reset to the fetched commit!
    fn fetch(&self) -> io::Result<()> {
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
        Ok(())
    }

    /// Push the local changes to remote.
    fn push(&self) -> io::Result<()> {
        self.run_git(&[
            "push",
            self.remote_name.as_str(),
            &format!(
                "{}:{}",
                self.base_commit.read().as_str(),
                self.branch_name.as_str()
            ),
        ])?;
        log::info!(
            "Pushed {} to {}/{}",
            self.base_commit.read().as_str(),
            self.remote_name.as_str(),
            self.branch_name.as_str()
        );
        *self.base_commit.write() = self.base_commit_from_remote();
        Ok(())
    }

    fn git(&self) -> GitCommand {
        GitCommand::at(&self.repo_path)
    }

    fn run_git(&self, args: &[&str]) -> io::Result<()> {
        let _out = GitCommand::at(&self.repo_path).args(args).output()?;
        Ok(())
    }

    /// Commit changes on base_commit. Update base_commit. Return the commit hash (hex).
    fn commit(&self, files: &[(String, Option<&str>)], message: &str) -> io::Result<String> {
        let payload = fast_import_payload(self, files, message);
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
                format!("fastimport failed (exit code: {})", code),
            ));
        }
        drop(tmp_file);

        let commit_oid = commit_oid.trim().to_string();
        if commit_oid.is_empty() {
            return notebackend_types::error::invalid_input(
                "fastimport did not provide new commit hash",
            );
        }
        log::info!("Committed: {}", &commit_oid);
        *self.base_commit.write() = commit_oid.clone();
        Ok(commit_oid)
    }
}

impl GitTextIO {
    /// Path for the text object.
    fn text_path(&self, id: Id) -> String {
        format!("notes/{:x}/{:x}", id / 256, id % 256)
    }

    fn read_text_from_git(&self, id: Id) -> io::Result<String> {
        let path = self.text_path(id);
        let data = match self.read_path_utf8(&path) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => String::new(),
            Ok((_oid, data)) => data,
            Err(e) => return Err(e),
        };
        Ok(data)
    }

    /// Load the metadata (parents, children, meta).
    /// Updates `last_manifest` state.
    fn load_manifest(&mut self) -> io::Result<Manifest> {
        let (oid, manifest_str) = match self.read_path_utf8(MANIFEST_NAME) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => (None, "{}".to_string()),
            Ok((oid, s)) => (Some(oid), s),
            Err(e) => return Err(e),
        };
        let mut manifest: Manifest = serde_json::from_str(&manifest_str)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        manifest.next_id = manifest.next_id.max(min_next_id());
        manifest.rebuild_parents();
        self.last_manifest = manifest.clone();
        Ok(manifest)
    }

    /// Read a utf-8 file. Return (oid, text).
    fn read_path_utf8(&self, file_path: &str) -> io::Result<(HexOid, String)> {
        let spec = format!("{}:{}", self.info.base_commit.read().as_str(), file_path);
        let (oid, data) = self.read_object(&spec, Some("blob"))?;
        Ok((oid, String::from_utf8_lossy(&data).into_owned()))
    }

    /// Read an object (git-cat). Return (oid, data).
    fn read_object(
        &self,
        spec: &str,
        expected_type: Option<&str>,
    ) -> io::Result<(HexOid, Vec<u8>)> {
        let mut object_reader = self.object_reader.lock();
        if object_reader.is_none() {
            let child = self
                .info
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
                    .info
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
                format!("git object {} is not found", spec),
            ));
        }
        if first_line.contains("ambiguous") {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("git object {} is ambiguous", spec),
            ));
        }
        let (oid, obj_type, len) = {
            let split: Vec<&str> = first_line.split(" ").collect();
            if split.len() != 3 || split[0].len() != 40 || split[2].parse::<usize>().is_err() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "cat-file {} returned malformed header: {:?}",
                        spec, first_line
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
                    format!(
                        "git object {} has type {}, but expect {}",
                        spec, obj_type, expected_type
                    ),
                ));
            }
        }
        let mut data = vec![0u8; len + 1];
        stdout.read_exact(&mut data)?;
        if data.last() != Some(&b'\n') {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("cat-file {} returned malformed end", spec),
            ));
        }
        data.pop(); // Remove the last LF.
        Ok((HexOid(oid.to_string()), data))
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
    fn commit_with_manifest(&mut self, manifest: &mut Manifest) -> io::Result<Option<String>> {
        // Nothing changed?
        if self.optimize_then_count_changed_files(manifest)? == 0 {
            if &self.last_manifest == manifest {
                log::info!("Nothing changed - No need to commit");
                return Ok(None);
            } else {
                log::debug!("Manifest has changed");
            }
        }

        self.with_changed_files(manifest, |files| {
            let commit_oid = self.info.commit(&files, "FooNote Checkpoint")?;
            Ok(Some(commit_oid))
        })
    }

    /// Calculate "changed files" (path, Some(data) /* write */ | None /* delete */)
    /// for committing.
    fn with_changed_files<R>(
        &self,
        manifest: &Manifest,
        f: impl FnOnce(&[(String, Option<&str>)]) -> R,
    ) -> R {
        let manifest_str =
            serde_json::to_string_pretty(&manifest).expect("Manifest can be encoded");
        let texts = self.texts.read();
        let files: Vec<(String, Option<&str>)> =
            std::iter::once((MANIFEST_NAME.to_string(), Some(manifest_str.as_str())))
                .chain(texts.iter().filter_map(|(&id, data)| {
                    match data {
                Some((data, true /* modified */ )) => Some((self.text_path(id), Some(data.as_str()))),
                None /* deletion */ => Some((self.text_path(id), None)),
                Some((data, false /* not modified */ )) => None,
            }
                }))
                .collect();
        f(&files)
    }
}

/// Prepare payload for git fastimport.
fn fast_import_payload(
    info: &GitInfo,
    // (path, content)
    files: &[(String, Option<&str>)],
    // commit message
    message: &str,
) -> String {
    // Prepare fast-import payload.
    let mut payload = String::with_capacity(4096);

    // Prepare the blobs.
    for (i, &(ref path, text)) in files.iter().enumerate() {
        if let Some(text) = text {
            let blob = fast_import_blob(text, i);
            payload += &blob;
        }
    }

    // Prepare the commit object.
    let local_ref_name = format!("next-{}-{}", info.remote_name, info.branch_name);
    let when = format!("{} +0000", epoch());
    let file_modifications = {
        let mut modifications = vec![];
        for (i, &(ref path, text)) in files.iter().enumerate() {
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
    let commit = format!(
        concat!(
            "commit refs/tags/{ref_name}\n",
            "mark :{commit_id}\n",
            "committer {name} <{email}> {when}\n",
            "data {message_len}\n{message}\n",
            "from {from}\n",
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
        from = info.base_commit.read().as_str(),
        files = file_modifications.concat(),
    );
    payload += &commit;

    payload
}

impl Drop for GitTextIO {
    fn drop(&mut self) {
        let mut object_reader = self.object_reader.lock();
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
    // origin  https://github.com/quark-zju/foonote (fetch)
    let text = GitCommand::at(repo_path).args(&["remote", "-v"]).output()?;

    // Check existing remote names.
    let mut taken_names = HashSet::new();
    for line in text.lines() {
        let split: Vec<&str> = line.split('\t').collect();
        if let Some(&[remote_name, remote_url]) = split.get(..2) {
            let remote_url = remote_url.strip_suffix(" (push)").unwrap_or(remote_url);
            let remote_url = remote_url.strip_suffix(" (fetch)").unwrap_or(remote_url);
            if remote_url == url {
                return Ok(remote_name.to_string());
            }
            taken_names.insert(remote_name.to_string());
        }
    }

    // Pick a new remote name.
    let base_name = {
        let mut s = DefaultHasher::new();
        url.hash(&mut s);
        format!("r{:x}", s.finish() & 0xfffff)
    };
    let remote_name = (0..)
        .map(|i| format!("{}{}", base_name, i))
        .filter(|n| !taken_names.contains(n))
        .next()
        .unwrap();

    // Create the new remote name.
    let _out = GitCommand::at(repo_path)
        .args(&["remote", "add", remote_name.as_str(), url])
        .context(format!(
            "create remote {} for url {} repo at {}",
            remote_name,
            url,
            repo_path.display()
        ))
        .output()?;

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
                format!("invalid url: {}", url),
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
            let out = match self.command(|c| c.output()) {
                Err(e) => {
                    return Err(self.error(format!("cannot spawn {} {:?} {}", GIT, &self.args, e)));
                }
                Ok(o) => o,
            };
            let stdout = String::from_utf8_lossy(&out.stdout);
            let stderr = String::from_utf8_lossy(&out.stderr);
            if !out.status.success() {
                let code = out.status.code().unwrap_or_default();
                log::warn!("{} {:?} exited {}", GIT, &self.args, code);
                let at = self
                    .git_dir
                    .as_ref()
                    .map(|s| s.display().to_string())
                    .unwrap_or_else(|| ".".to_string());
                return Err(self.error(format!(
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

            Ok(stdout.to_string())
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
                log::info!("running {} {:?}", GIT, &self.args);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::tests::*;
    use crate::clipboard;
    use notebackend_types::TreeBackend;

    fn populate_git_repo(dir: &Path) -> Option<PathBuf> {
        let git_repo_path = dir.join("repo");
        let git_dir = git_repo_path.join(".git");

        let git_repo_path_str = git_repo_path.display().to_string();
        match GitCommand::default()
            .args(&["-c", "init.defaultBranch=m", "init", &git_repo_path_str])
            .run()
            .ok()
        {
            None => {
                eprintln!("skip test - git init does not work properly");
                return None;
            }
            Some(_) => {}
        }

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
}
