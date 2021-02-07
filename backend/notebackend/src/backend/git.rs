use super::Id;
use super::InsertPos;
use super::Mtime;
use super::TreeBackend;
use once_cell::sync::Lazy;
use parking_lot::lock_api::RwLockUpgradableReadGuard;
use parking_lot::Mutex;
use parking_lot::RwLock;
use serde::Deserialize;
use serde::Serialize;
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
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
use std::process::Command;
use std::process::ExitStatus;
use std::process::Stdio;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;
use tempfile::NamedTempFile;

// Repo structure:
// - text/{id}
// - manifest.json: {children: {id: [id]}, meta: {id: str}}

/// Git backend using the system git binary.
pub struct GitBackend {
    remote_name: String,
    repo_path: PathBuf,
    branch_name: String,
    // value of texts: Some((content, modified)) | None (deleted)
    texts: RwLock<HashMap<Id, Option<(String, bool)>>>,
    mtime: HashMap<Id, Mtime>,
    object_reader: Mutex<Option<Child>>,
    base_commit: String,
    manifest: Manifest,
    user: Lazy<String>,
    email: Lazy<String>,
}

#[derive(Serialize, Deserialize, Default)]
struct Manifest {
    #[serde(default)]
    children: BTreeMap<Id, Vec<Id>>,
    #[serde(default)]
    metas: BTreeMap<Id, String>,
    #[serde(default = "default_next_id")]
    next_id: Id,
    #[serde(skip)]
    parents: HashMap<Id, Id>, // derived from children
}

const MANIFEST_NAME: &str = "manifest.json";

#[derive(Clone, Debug)]
struct HexOid(String);

impl GitBackend {
    /// Creates a Git backend. `url` specifies the repo location.
    /// `cache_dir` can be None, which means using the system default cache
    /// location, or a specified location for testing purpose.
    pub fn new(url: &str, cache_dir: Option<&Path>) -> io::Result<GitBackend> {
        let (url, hash) = split_url(url)?;
        let repo_path = prepare_bare_staging_repo(cache_dir)?;
        let remote_name = prepare_remote_name(&repo_path, url)?;
        let branch_name = hash.unwrap_or("master").to_string();
        let base_commit = format!("{}/{}", &remote_name, &branch_name);
        let mut backend = Self {
            remote_name,
            repo_path,
            branch_name,
            base_commit,
            texts: Default::default(),
            mtime: Default::default(),
            object_reader: Default::default(),
            manifest: Default::default(),
            user: Lazy::new(|| git_config("user.name").unwrap_or_else(|| "FooNote".to_string())),
            email: Lazy::new(|| {
                git_config("user.email").unwrap_or_else(|| "foonote@example.com".to_string())
            }),
        };
        backend.fetch()?;
        backend.load_manifest()?;
        Ok(backend)
    }
}

impl TreeBackend for GitBackend {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> io::Result<Vec<Self::Id>> {
        Ok(self.manifest.children.get(&id).cloned().unwrap_or_default())
    }

    fn get_parent(&self, id: Self::Id) -> io::Result<Option<Self::Id>> {
        Ok(self.manifest.parents.get(&id).cloned())
    }

    fn get_mtime(&self, id: Self::Id) -> io::Result<Mtime> {
        Ok(self.mtime.get(&id).cloned().unwrap_or_default())
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        let text = self.texts.upgradable_read();
        match text.get(&id) {
            Some(Some((data, _modified))) => Ok(data.clone().into()),
            Some(None) => {
                // Was deleted.
                Ok("".into())
            }
            None => {
                let mut text = RwLockUpgradableReadGuard::upgrade(text);
                let path = self.text_path(id);
                let data = match self.read_path_utf8(&path) {
                    Err(e) if e.kind() == io::ErrorKind::NotFound => String::new(),
                    Ok((_oid, data)) => data,
                    Err(e) => return Err(e),
                };
                text.insert(id, Some((data.clone(), false)));
                Ok(data.into())
            }
        }
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        Ok(self
            .manifest
            .metas
            .get(&id)
            .cloned()
            .unwrap_or_default()
            .into())
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: InsertPos,
        text: String,
        meta: String,
    ) -> io::Result<Self::Id> {
        let id = self.manifest.next_id;
        self.manifest.next_id += 1;
        let parent_id = match pos {
            InsertPos::Append => dest_id,
            InsertPos::Before | InsertPos::After => self
                .get_parent(dest_id)?
                .unwrap_or_else(|| self.get_root_id()),
        };
        let children = self.manifest.children.entry(parent_id).or_default();
        let index = match pos {
            InsertPos::Append => -1isize,
            InsertPos::Before => children.iter().position(|&c| c == dest_id).unwrap_or(0) as isize,
            InsertPos::After => {
                children
                    .iter()
                    .position(|&c| c == dest_id)
                    .map(|i| i as isize)
                    .unwrap_or(-2)
                    + 1
            }
        };
        if index < 0 || index as usize >= children.len() {
            children.push(id);
        } else {
            children.insert(index as usize, id);
        }
        self.manifest.parents.insert(id, parent_id);
        if !text.is_empty() {
            self.set_text(id, text)?;
        }
        if !meta.is_empty() {
            self.set_raw_meta(id, meta)?;
        }
        self.touch(id)?;
        Ok(id)
    }

    fn set_parent(
        &mut self,
        id: Self::Id,
        dest_id: Self::Id,
        pos: InsertPos,
    ) -> io::Result<Self::Id> {
        let parent_id = match pos {
            InsertPos::Before | InsertPos::After => self
                .get_parent(dest_id)?
                .unwrap_or_else(|| self.get_root_id()),
            InsertPos::Append => dest_id,
        };
        if self.is_ancestor(id, parent_id)? {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "{:?} ({}) cannot be moved to be under its descendant {:?} ({})",
                    self.get_text_first_line(id).unwrap_or_default(),
                    id,
                    self.get_text_first_line(parent_id).unwrap_or_default(),
                    dest_id,
                ),
            ));
        }
        self.touch(id)?;
        self.manifest.remove_parent(id);
        self.manifest.parents.insert(id, parent_id);
        let children = self.manifest.children.entry(parent_id).or_default();
        let index = match pos {
            InsertPos::Append => -1,
            InsertPos::Before => children
                .iter()
                .position(|&i| i == dest_id)
                .map(|i| i as isize)
                .unwrap_or(0),
            InsertPos::After => {
                children
                    .iter()
                    .position(|&i| i == dest_id)
                    .map(|i| i as isize)
                    .unwrap_or(-2)
                    + 1
            }
        };
        if index < 0 || index as usize > children.len() {
            children.push(id);
        } else {
            children.insert(index as usize, id);
        }
        self.touch(id)?;
        Ok(id)
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> io::Result<()> {
        let orig_text = self.get_text(id)?;
        if orig_text.as_ref() != text.as_str() {
            let mut texts = self.texts.write();
            texts.insert(id, Some((text, true)));
            drop(texts);
            self.touch(id)?;
        }
        Ok(())
    }

    fn set_raw_meta(&mut self, id: Self::Id, meta: String) -> io::Result<()> {
        let orig_meta = self.get_raw_meta(id)?;
        if orig_meta != meta {
            self.manifest.metas.insert(id, meta);
            self.touch(id)?;
        }
        Ok(())
    }

    fn remove(&mut self, id: Self::Id) -> io::Result<()> {
        self.touch(id)?;
        let parent = self.get_parent(id)?;
        self.manifest.remove_parent(id);
        self.manifest.remove(id);
        let mut texts = self.texts.write();
        texts.entry(id).and_modify(|e| *e = None);
        Ok(())
    }

    fn persist(&mut self) -> io::Result<()> {
        self.commit()?;
        self.push()?;
        self.texts.write().clear();
        Ok(())
    }
}

impl Manifest {
    /// Rebuild parents from children data.
    fn rebuild_parents(&mut self) {
        for (&id, child_ids) in &self.children {
            for &child_id in child_ids {
                self.parents.insert(child_id, id);
            }
        }
    }

    /// Remove an id from its parent's children list.
    fn remove_parent(&mut self, id: Id) {
        if let Some(parent_id) = self.parents.get(&id) {
            if let Some(children) = self.children.get_mut(parent_id) {
                if let Some(pos) = children.iter().position(|x| *x == id) {
                    children.remove(pos);
                }
                if children.is_empty() {
                    self.children.remove(parent_id);
                }
            }
        }
        self.parents.remove(&id);
    }

    fn remove(&mut self, id: Id) {
        self.parents.remove(&id);
        self.children.remove(&id);
        self.metas.remove(&id);
    }
}

impl GitBackend {
    /// Fetch the latest commit from the remote.
    fn fetch(&self) -> io::Result<()> {
        self.run_git(&[
            "fetch",
            self.remote_name.as_str(),
            self.branch_name.as_str(),
        ])
    }

    /// Bump mtime of id and its parents.
    fn touch(&mut self, mut id: Id) -> io::Result<()> {
        loop {
            self.mtime.entry(id).and_modify(|v| *v += 1).or_insert(1);
            if let Some(parent_id) = self.get_parent(id)? {
                if parent_id != id {
                    id = parent_id;
                    continue;
                }
            }
            break;
        }
        Ok(())
    }

    /// Path for the text object.
    fn text_path(&self, id: Id) -> String {
        format!("text/{:x}/{:x}", id / 256, id % 256)
    }

    /// Load the metadata (parents, children, meta).
    fn load_manifest(&mut self) -> io::Result<()> {
        let (oid, manifest_str) = match self.read_path_utf8(MANIFEST_NAME) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => (None, "{}".to_string()),
            Ok((oid, s)) => (Some(oid), s),
            Err(e) => return Err(e),
        };
        let mut manifest: Manifest = serde_json::from_str(&manifest_str)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        manifest.next_id = manifest.next_id.max(default_next_id());
        manifest.rebuild_parents();
        self.manifest = manifest;
        Ok(())
    }

    fn run_git(&self, args: &[&str]) -> io::Result<()> {
        let status = git_command()
            .arg("--git-dir")
            .arg(&self.repo_path)
            .current_dir(&self.repo_path)
            .args(args)
            .status()?;
        check_status(status, || format!("running {:?}", args))?;
        Ok(())
    }

    /// Read a utf-8 file. Return (oid, text).
    fn read_path_utf8(&self, file_path: &str) -> io::Result<(HexOid, String)> {
        let spec = format!("{}:{}", &self.base_commit, file_path);
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
            let child = git_command()
                .arg("--git-dir")
                .arg(&self.repo_path)
                .current_dir(&self.repo_path)
                .args(&["cat-file", "--batch"])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()?;
            *object_reader = Some(child);
        }
        let reader = object_reader.as_mut().expect("child created above");
        let stdin = reader
            .stdin
            .as_mut()
            .expect("child crated with piped stdin");
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

    /// Commit changes. Return the hex commit hash.
    /// Update self.base_commit to the new commit.
    fn commit(&mut self) -> io::Result<String> {
        let payload = self.fast_import_payload();
        let mut tmp_file = NamedTempFile::new_in(&self.repo_path)?;
        tmp_file.write_all(&payload.as_bytes())?;
        tmp_file.flush()?;
        let stdin_file = File::open(tmp_file.path())?;

        let mut process = git_command()
            .arg("--git-dir")
            .arg(&self.repo_path)
            .current_dir(&self.repo_path)
            .args(&["fast-import", "--quiet", "--force"])
            .stdin(Stdio::from(stdin_file))
            .stdout(Stdio::piped())
            .spawn()?;

        let stdout = process.stdout.as_mut().expect("piped stdin");
        let mut commit_oid = String::new();
        stdout.read_to_string(&mut commit_oid)?;

        let status = process.wait()?;
        check_status(status, || "fastimport failed".to_string())?;
        drop(tmp_file);

        let commit_oid = commit_oid.trim().to_string();
        if commit_oid.is_empty() {
            return notebackend_types::error::invalid_input(
                "fastimport did not provide new commit hash",
            );
        }
        self.base_commit = commit_oid.clone();
        Ok(commit_oid)
    }

    /// Push the local changes to remote.
    fn push(&self) -> io::Result<()> {
        self.run_git(&[
            "push",
            self.remote_name.as_str(),
            &format!(
                "{}:{}",
                self.base_commit.as_str(),
                self.branch_name.as_str()
            ),
        ])
    }

    /// Fast-import payload for commit().
    fn fast_import_payload(&self) -> String {
        // Prepare fast-import payload.
        let mut payload = String::with_capacity(4096);

        // Prepare the blobs.
        let texts = self.texts.read();
        for (&id, text) in texts.iter() {
            if let Some((text, true /* modified */)) = text {
                let blob = fast_import_blob(text, id);
                payload += &blob;
            }
        }

        // Prepare the manifest.
        let manifest =
            serde_json::to_string_pretty(&self.manifest).expect("Manifest can be encoded");
        let manifest_id = self.manifest.next_id + ID_MARK_OFFSET;
        let blob = fast_import_blob(&manifest, manifest_id);
        payload += &blob;

        // Prepare the commit object.
        let local_ref_name = format!("next-{}-{}", &self.remote_name, &self.branch_name);
        let when = format!("{} +0000", epoch());
        let file_modifications = {
            let mut modifications = vec![fast_import_file_modify(MANIFEST_NAME, manifest_id)];
            for (&id, text) in texts.iter() {
                match text {
                    Some((text, true)) => {
                        let path = self.text_path(id);
                        modifications.push(fast_import_file_modify(&path, id));
                    }
                    None => {
                        let path = self.text_path(id);
                        modifications.push(fast_import_file_delete(&path));
                    }
                    Some((_, false)) => { /* skip - not modified */ }
                }
            }
            modifications
        };

        let commit_id = manifest_id + 1;
        let message = "FooNote Checkpoint";
        let commit = format!(
            concat!(
                "commit {ref_name}\n",
                "mark :{commit_id}\n",
                "committer {name} <{email}> {when}\n",
                "data {message_len}\n{message}\n",
                "from {from}\n",
                "{files}",
                "\n",
                "get-mark :{commit_id}\n",
                "reset refs/tags/{ref_name}\n",
                "from :{commit_id}\n",
                "\n",
            ),
            ref_name = local_ref_name,
            commit_id = commit_id,
            name = self.user.as_str(),
            email = self.email.as_str(),
            when = when,
            message_len = message.len(),
            message = &message,
            from = &self.base_commit,
            files = file_modifications.concat(),
        );
        payload += &commit;

        payload
    }
}

impl Drop for GitBackend {
    fn drop(&mut self) {
        let mut object_reader = self.object_reader.lock();
        if let Some(ref mut child) = *object_reader {
            // Close stdin pipe.
            child.stdin = None;
            // Wait for the process to end.
            let _ = child.wait();
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

fn git_command() -> Command {
    Command::new("git")
}

fn git_config(key: &str) -> Option<String> {
    let output = git_command()
        .args(&["config", "--get", key])
        .output()
        .ok()?;
    if output.status.success() {
        let s = String::from_utf8_lossy(&output.stdout).trim().into();
        Some(s)
    } else {
        None
    }
}

/// Create a remote pointing to the url on demand. Return the remote name.
fn prepare_remote_name(repo_path: &Path, url: &str) -> io::Result<String> {
    // origin  https://github.com/quark-zju/foonote (fetch)
    let output = git_command()
        .arg("--git-dir")
        .arg(repo_path)
        .current_dir(repo_path)
        .args(&["remote", "-v"])
        .output()?;

    // Check existing remote names.
    let text = String::from_utf8_lossy(&output.stdout);
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
    let status = git_command()
        .arg("--git-dir")
        .arg(repo_path)
        .current_dir(repo_path)
        .args(&["remote", "add", remote_name.as_str(), url])
        .status()?;
    check_status(status, || {
        format!(
            "create remote {} for url {} repo at {}",
            remote_name,
            url,
            repo_path.display()
        )
    })?;

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
    let status = git_command()
        .args(&["init", "--bare"])
        .arg(&path)
        .status()?;
    check_status(status, || format!("create bare repo at {}", path.display()))?;
    Ok(path)
}

fn check_status(status: ExitStatus, msg: impl Fn() -> String) -> io::Result<()> {
    if !status.success() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("git exited {}: {}", status.code().unwrap_or(-1), msg()),
        ))
    } else {
        Ok(())
    }
}

const ID_MARK_OFFSET: Id = 1;

fn fast_import_blob(text: &str, id: Id) -> String {
    format!(
        "blob\nmark :{}\ndata {}\n{}\n",
        id + ID_MARK_OFFSET,
        text.len(),
        text
    )
}

fn fast_import_file_modify(path: &str, id: Id) -> String {
    format!("M 100644 :{} {}\n", id + ID_MARK_OFFSET, path)
}

fn fast_import_file_delete(path: &str) -> String {
    format!("D {}\n", path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::clipboard;
    use crate::backend::tests::*;

    fn populate_git_repo(dir: &Path) -> Option<PathBuf> {
        let git_repo_path = dir.join("repo");
        let git_dir = git_repo_path.join(".git");

        match git_command().arg("init").arg(&git_repo_path).status().ok() {
            None => return None, // skip test: git does not work.
            Some(s) if !s.success() => return None,
            Some(s) => {}
        }

        // Make a commit on the master branch.
        std::fs::write(git_repo_path.join("x"), b"x").unwrap();
        let git = |args: &[&str]| {
            git_command()
                .arg("--git-dir")
                .arg(&git_dir)
                .args(args)
                .current_dir(&git_repo_path)
                .status()
                .unwrap();
        };
        git(&["add", "x"]);
        git(&["commit", "-m", "add x"]);
        git(&["checkout", "-b", "trunk"]);
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

        let url = format!("{}#trunk", git_repo_path.display());
        let mut backend = GitBackend::new(&url, Some(&cache_path)).unwrap();
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
        let backend = GitBackend::new(&url, Some(&cache_path)).unwrap();
        check(backend);

        // Test re-load from the original repo using the a different cache.
        let cache_path = dir.path().join("cache2");
        let backend = GitBackend::new(&url, Some(&cache_path)).unwrap();
        check(backend);
    }
}
