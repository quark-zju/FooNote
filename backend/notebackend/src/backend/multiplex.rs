//! Multiplex backend. Mixing multiple backends together.

use super::null::NullBackend;
use crate::clipboard;
use notebackend_types::meta::TreeMeta;
use notebackend_types::BackendId;
use notebackend_types::Id;
use notebackend_types::InsertPos;
use notebackend_types::Mtime;
use notebackend_types::PersistCallbackFunc;
use notebackend_types::TreeBackend;
use parking_lot::Mutex;
use parking_lot::RwLock;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io;
use std::io::Result;
use std::sync::Arc;

pub type FullId = (BackendId, Id);
type BoxBackend = Box<dyn TreeBackend<Id = Id>>;

const ROOT_BACKEND_ID: BackendId = 0;

/// Multiplex backend. A backend node can mount other backend roots.
#[derive(Default)]
pub struct MultiplexBackend {
    /// Backends, referred by BackendId. The first one is the root backend.
    backends: Vec<BoxBackend>,

    /// Mount overrides. Original Id -> New mounted Id (usually root).
    table_srcdst: HashMap<FullId, FullId>,

    /// Reverse of `table_srcdst`. Useful to lookup parents.
    table_dstsrc: HashMap<FullId, FullId>,
}

impl TreeBackend for MultiplexBackend {
    type Id = FullId;

    fn get_children(&self, id: Self::Id) -> Result<Vec<Self::Id>> {
        // Follow mount points.
        let id = self.table_srcdst.get(&id).cloned().unwrap_or(id);
        let children = self.get_backend(id.0)?.get_children(id.1)?;
        Ok(children.into_iter().map(|c| (id.0, c)).collect())
    }

    fn get_parent(&self, id: Self::Id) -> Result<Option<Self::Id>> {
        let backend = self.get_backend(id.0)?;
        let parent = backend.get_parent(id.1)?.map(|p| {
            let id = (id.0, p);
            self.table_dstsrc.get(&id).cloned().unwrap_or(id)
        });
        Ok(parent)
    }

    fn get_mtime(&self, id: Self::Id) -> Result<Mtime> {
        self.get_backend(id.0)?.get_mtime(id.1)
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> Result<std::borrow::Cow<'a, str>> {
        self.get_backend(id.0)?.get_text(id.1)
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> Result<std::borrow::Cow<'a, str>> {
        self.get_backend(id.0)?.get_raw_meta(id.1)
    }

    fn insert(
        &mut self,
        mut dest_id: Self::Id,
        pos: InsertPos,
        text: String,
        meta: String,
    ) -> Result<Self::Id> {
        match pos {
            InsertPos::Append => {
                // Follow mount points.
                dest_id = self.table_srcdst.get(&dest_id).cloned().unwrap_or(dest_id);
            }
            _ => {}
        }
        let backend = self.get_backend_mut(dest_id.0)?;
        let id = backend
            .insert(dest_id.1, pos, text, meta)
            .map(|i| (dest_id.0, i))?;
        self.bump_parent_mtime(id)?;
        Ok(id)
    }

    fn set_parent(&mut self, id: Self::Id, dest_id: Self::Id, pos: InsertPos) -> Result<Self::Id> {
        if id.0.max(dest_id.0) >= self.backends.len() {
            return notebackend_types::error::invalid_input("backend id is out of bound");
        }
        if id.0 == dest_id.0 {
            // Move within a single backend.
            let dst = self.get_backend_mut(dest_id.0)?;
            dst.set_parent(id.1, dest_id.1, pos)?;
            self.bump_parent_mtime(id)?;
            Ok(id)
        } else {
            // We need a "&"", and a "&mut" from the same vec. Make borrowck happy.
            let mut src: BoxBackend = Box::new(NullBackend);
            let mut dst: BoxBackend = Box::new(NullBackend);
            // Bound checks were done above.
            self.swap_backend(id.0, &mut src);
            self.swap_backend(dest_id.0, &mut dst);
            let result = (|| -> Result<Self::Id> {
                // Do not move special nodes.
                if !src.is_copyable(id.1)? {
                    return notebackend_types::error::invalid_input(
                        "cannot copy non-copyable nodes",
                    );
                }
                // Do the move.
                let dst_id = dst.insert(dest_id.1, pos, String::new(), String::new())?;
                clipboard::copy_replace(&src, id.1, &mut dst, dst_id, None)?;
                src.remove(id.1)?;
                Ok((dest_id.0, dst_id))
            })();
            // Swap back.
            self.swap_backend(id.0, &mut src);
            self.swap_backend(dest_id.0, &mut dst);
            self.bump_parent_mtime(id)?;
            self.bump_parent_mtime(dest_id)?;
            result
        }
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> Result<bool> {
        let backend = self.get_backend_mut(id.0)?;
        let changed = backend.set_text(id.1, text)?;
        if changed {
            self.bump_parent_mtime(id)?;
        }
        Ok(changed)
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> Result<bool> {
        let backend = self.get_backend_mut(id.0)?;
        let changed = backend.set_raw_meta(id.1, content)?;
        if changed {
            self.bump_parent_mtime(id)?;
        }
        Ok(changed)
    }

    fn touch(&mut self, id: Self::Id) -> Result<()> {
        log::debug!("touch {:?}", id);
        let backend = self.get_backend_mut(id.0)?;
        backend.touch(id.1)?;
        self.bump_parent_mtime(id)?;
        Ok(())
    }

    fn remove(&mut self, id: Self::Id) -> Result<()> {
        let mut to_umount = Vec::new();
        for &mount_id in self.table_srcdst.keys() {
            if self.is_ancestor(id, mount_id)? {
                to_umount.push(mount_id);
                log::info!("umount {:?} on deletion", mount_id);
            }
        }
        to_umount.sort_unstable();
        for id in to_umount.into_iter().rev() {
            self.umount(id)?;
        }

        let backend = self.get_backend_mut(id.0)?;
        backend.remove(id.1)?;
        self.bump_parent_mtime(id)?;
        Ok(())
    }

    fn persist(&mut self) -> Result<()> {
        let mut err = None;
        for backend in self.backends.iter_mut().rev() {
            match backend.persist() {
                Ok(_) => {}
                // Continue persisting other backends.
                Err(e) => err = Some(e),
            }
        }
        match err {
            None => Ok(()),
            Some(e) => Err(e),
        }
    }

    fn persist_async(&mut self, callback: PersistCallbackFunc) {
        let count = self.backends.len();
        let results: Arc<Mutex<Vec<io::Result<()>>>> = Default::default();
        let callback = Arc::new(Mutex::new(Some(callback)));
        let urls: Vec<String> = self
            .backends
            .iter()
            .enumerate()
            .map(|(i, b)| {
                match self
                    .table_dstsrc
                    .get(&(i, b.get_root_id()))
                    .and_then(|id| self.extract_meta(*id, "mount=").ok())
                {
                    Some(url) => url.to_string(),
                    None => String::new(),
                }
            })
            .collect();
        for (backend, url) in self.backends.iter_mut().zip(urls) {
            let results = results.clone();
            let callback = callback.clone();
            backend.persist_async(Box::new(move |mut r| {
                if url.is_empty() {
                    r = r.map_err(|e| io::Error::new(e.kind(), format!("{} ({})", e, url)));
                }
                let mut results = results.lock();
                results.push(r);
                if results.len() == count {
                    let single_result =
                        results
                            .drain(..)
                            .fold(Ok(()), |acc, item| match (&acc, &item) {
                                (Ok(_), _) => item,
                                (Err(_), _) => acc,
                            });
                    if let Some(callback) = callback.lock().take() {
                        callback(single_result);
                    }
                }
            }));
        }
    }
}

/// Based on another backend. Treat "mount=" nodes specially by "mounting"
/// other backends into those nodes.
pub struct MountableBackend {
    root: Mount,
    table: RwLock<MountTable>,
}

#[derive(Default)]
struct MountTable {
    /// Backends, referred by BackendId. The first one is the root backend.
    mounts: Vec<Mount>,

    /// Url to backend Id.
    url_to_id: HashMap<String, BackendId>,
}

struct Mount {
    backend: BoxBackend,
    error_message: Option<String>,

    /// The node that has the "mount=url" meta.
    source_id: FullId,
    url: Option<String>,
}

impl Default for MountableBackend {
    fn default() -> Self {
        Self::from_root_backend(Box::new(NullBackend))
    }
}

impl MountableBackend {
    pub fn from_root_backend(backend: BoxBackend) -> Self {
        Self {
            root: Mount {
                backend,
                error_message: None,
                source_id: (0, 0),
                url: None,
            },
            table: Default::default(),
        }
    }

    // Mount `id` on demand. Return the mounted id.
    fn maybe_mount(&self, id: FullId) -> io::Result<FullId> {
        // Root id cannot be mounted.
        if id == self.get_root_id() {
            return Ok(id);
        }
        let url = self.extract_url(id)?;
        if url.is_empty() {
            // No URL to mount.
            return Ok(id);
        }

        let mut table = self.table.write();
        if let Some(&backend_index) = table.url_to_id.get(url.as_ref()) {
            // Already mounted.
            log::trace!("Reuse backend {} for URL {}", backend_index, &url,);
            let mount = &table.mounts[backend_index];
            let root_id = mount.backend.get_root_id();
            if mount.source_id != id {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Cannot mount {} in multiple places", url),
                ));
            }
            let backend_id = backend_index + 1;
            return Ok((backend_id, root_id));
        }

        log::debug!("Attempt to mount {} at {:?}", &url, id);
        let mut error_message = None;
        let backend = match crate::url::open(url.as_ref()) {
            Ok(backend) => backend,
            Err(e) => {
                log::warn!("Failed to mount {}: {:?}", &url, e);
                error_message = Some(e.to_string());
                Box::new(NullBackend)
            }
        };
        let root_id = backend.get_root_id();

        // Find an umount entry.
        let backend_index = table
            .mounts
            .iter()
            .enumerate()
            .filter_map(|(i, m)| if m.url.is_none() { Some(i) } else { None })
            .next();
        let mount = Mount {
            backend,
            error_message,
            source_id: id,
            url: Some(url.to_string()),
        };
        let backend_index = match backend_index {
            None => {
                let backend_index = table.mounts.len();
                table.mounts.push(mount);
                backend_index
            }
            Some(i) => {
                table.mounts[i] = mount;
                i
            }
        };
        table.url_to_id.insert(url.to_string(), backend_index);

        let backend_id = backend_index + 1;
        log::info!("Mounted {} for {:?} as backend {}", &url, id, backend_id);

        Ok((backend_id, root_id))
    }

    /// Unmount descendants of `id`.
    fn umount_recursive(&mut self, id: FullId) -> io::Result<()> {
        let mut table = self.table.write();
        let mut to_umount_index = Vec::new();
        for (i, mount) in table.mounts.iter().enumerate() {
            if self.is_ancestor(id, mount.source_id)? {
                to_umount_index.push(i);
            }
        }

        for i in to_umount_index.into_iter().rev() {
            let mount = &mut table.mounts[i];
            let url = match mount.url.as_ref() {
                Some(url) => url.clone(),
                None => continue,
            };
            let source_id = mount.source_id;
            // Attempt to avoid losing unsaved content.
            mount.backend.persist()?;
            mount.backend = Box::new(NullBackend);
            mount.url = None;
            mount.error_message = Some("umounted".to_string());
            table.url_to_id.remove(&url);
            log::info!("Umounted backend {} ({}) from {:?}", i + 1, &url, source_id);
        }

        Ok(())
    }

    fn with_mount<R>(
        &self,
        id: FullId,
        func: impl FnOnce(&Mount, Id) -> io::Result<R>,
    ) -> io::Result<R> {
        let (backend_id, id) = id;
        if backend_id == 0 {
            // Root backend - no need to lock.
            func(&self.root, id)
        } else {
            let table = self.table.read();
            if let Some(mount) = table.mounts.get(backend_id - 1) {
                func(mount, id)
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "backend out of bound",
                ));
            }
        }
    }

    fn with_mount_mut<R>(
        &mut self,
        id: FullId,
        func: impl FnOnce(&mut Mount, Id) -> io::Result<R>,
    ) -> io::Result<R> {
        let (backend_id, id) = id;
        if backend_id == 0 {
            // Root backend - no need to lock.
            func(&mut self.root, id)
        } else {
            let mut table = self.table.write();
            if let Some(mount) = table.mounts.get_mut(backend_id - 1) {
                func(mount, id)
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "backend out of bound",
                ));
            }
        }
    }

    /// Swap out the backend with the given backend_id. This is to make
    /// borrowck happy in `set_parent`.
    /// Be sure to call this function again to swap back the backend!
    fn swap_backend(&mut self, backend_id: BackendId, mut backend: BoxBackend) -> BoxBackend {
        use std::mem::swap;
        if backend_id == 0 {
            swap(&mut backend, &mut self.root.backend);
        } else {
            let mut table = self.table.write();
            if let Some(m) = table.mounts.get_mut(backend_id) {
                std::mem::swap(&mut backend, &mut m.backend);
            }
        }
        backend
    }

    fn extract_url(&self, id: FullId) -> io::Result<Cow<str>> {
        self.extract_meta(id, "mount=")
    }
}

impl TreeBackend for MountableBackend {
    type Id = FullId;

    fn get_root_id(&self) -> Self::Id {
        (0, self.root.backend.get_root_id())
    }

    fn get_children(&self, id: Self::Id) -> Result<Vec<Self::Id>> {
        // Auto-mount `id`.
        let fid = self.maybe_mount(id)?;
        let children = self
            .with_mount(fid, |m, id| m.backend.get_children(id))?
            .into_iter()
            .map(|i| (fid.0, i))
            .collect();
        Ok(children)
    }

    fn get_parent(&self, id: Self::Id) -> Result<Option<Self::Id>> {
        let backend_id = id.0;
        self.with_mount(id, |m, id| match m.backend.get_parent(id)? {
            Some(id) => {
                let fid = if id == m.backend.get_root_id() {
                    // Rewrite root id into the non-root id in source.
                    m.source_id
                } else {
                    (backend_id, id)
                };
                Ok(Some(fid))
            }
            None => {
                if backend_id != m.source_id.0 && id == m.backend.get_root_id() {
                    // Across mount boundary.
                    self.get_parent(m.source_id)
                } else {
                    Ok(None)
                }
            }
        })
    }

    fn get_mtime(&self, id: Self::Id) -> Result<Mtime> {
        let backend_id = id.0;
        self.with_mount(id, |m, id| m.backend.get_mtime(id))
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> Result<Cow<'a, str>> {
        let backend_id = id.0;
        self.with_mount(id, |m, id| Ok(m.backend.get_text(id)?.to_string().into()))
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> Result<Cow<'a, str>> {
        let backend_id = id.0;
        self.with_mount(id, |m, id| {
            Ok(m.backend.get_raw_meta(id)?.to_string().into())
        })
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: InsertPos,
        text: String,
        meta: String,
    ) -> Result<Self::Id> {
        let dest_id = self.maybe_mount(dest_id)?;
        let backend_id = dest_id.0;
        let new_id = self.with_mount_mut(dest_id, |m, dest_id| {
            Ok((backend_id, m.backend.insert(dest_id, pos, text, meta)?))
        })?;
        self.touch(new_id)?;
        Ok(new_id)
    }

    fn set_parent(
        &mut self,
        src_id: Self::Id,
        dest_id: Self::Id,
        pos: InsertPos,
    ) -> Result<Self::Id> {
        let dest_id = self.maybe_mount(dest_id)?;
        if src_id.0 == dest_id.0 {
            // Move within a single backend.
            self.touch(src_id)?;
            let moved_id = self.with_mount_mut(dest_id, |m, dest_id| {
                m.backend.set_parent(src_id.1, dest_id, pos)
            })?;
            self.touch(src_id)?;
            Ok((src_id.0, moved_id))
        } else {
            self.touch(src_id)?;
            let src = Box::new(NullBackend);
            let mut src = self.swap_backend(src_id.0, src);
            // Do not move special nodes.
            if !src.is_copyable(src_id.1)? {
                return notebackend_types::error::invalid_input("cannot copy non-copyable nodes");
            }

            let moved_id_result = self.with_mount_mut(dest_id, |m, local_dest_id| {
                let dst = &mut m.backend;

                // Do the move.
                let dst_id = dst.insert(local_dest_id, pos, String::new(), String::new())?;
                clipboard::copy_replace(&src, src_id.1, dst, dst_id, None)?;
                src.remove(src_id.1)?;
                Ok((dest_id.0, local_dest_id))
            });

            // Swap back.
            self.swap_backend(src_id.0, src);
            self.touch(src_id)?;

            moved_id_result
        }
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> Result<bool> {
        let changed = self.with_mount_mut(id, |m, local_id| m.backend.set_text(local_id, text))?;
        if changed {
            self.touch(id)?;
        }
        Ok(changed)
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> Result<bool> {
        let changed =
            self.with_mount_mut(id, |m, local_id| m.backend.set_raw_meta(local_id, content))?;
        if changed {
            self.touch(id)?;
        }
        // NOTE: Should check umount here.
        Ok(changed)
    }

    fn remove(&mut self, id: Self::Id) -> Result<()> {
        self.with_mount_mut(id, |m, local_id| m.backend.remove(local_id))?;
        self.touch(id)?;
        if !self.is_ancestor(self.get_root_id(), id)? {
            log::trace!("{:?} is no longer reachable from root", id);
            self.umount_recursive(id)?;
        }
        Ok(())
    }

    /// Bump mtime across mount boundary.
    fn touch(&mut self, mut id: FullId) -> Result<()> {
        if id.0 != 0 {
            let mut table = self.table.write();
            while id.0 != 0 {
                log::debug!("touch {:?}", id);
                log::trace!("bumping mtime for {:?}", id);
                if let Some(mount) = table.mounts.get_mut(id.0 - 1) {
                    mount.backend.touch(id.1)?;
                    id = mount.source_id;
                } else {
                    break;
                }
            }
        }

        log::debug!("touch {:?}", id);
        self.root.backend.touch(id.1)?;
        Ok(())
    }

    fn persist(&mut self) -> Result<()> {
        let mut table = self.table.write();
        for mount in table.mounts.iter_mut().rev() {
            mount.backend.persist()?;
        }
        self.root.backend.persist()?;
        Ok(())
    }

    fn persist_async(&mut self, callback: PersistCallbackFunc) {
        struct State {
            callback: Option<PersistCallbackFunc>,
            result: Vec<(String, io::Result<()>)>,
            waiting_count: usize,
        }

        let mut table = self.table.write();
        let state = Arc::new(Mutex::new(State {
            callback: Some(callback),
            result: Default::default(),
            waiting_count: table.mounts.len() + 1,
        }));

        let process = |mount: &mut Mount| {
            let state = state.clone();
            let url = mount.url.clone().unwrap_or_else(|| "".to_string());
            mount.backend.persist_async(Box::new(move |r| {
                let mut state = state.lock();
                state.result.push((url, r));
                state.waiting_count -= 1;
                if state.waiting_count == 0 {
                    if let Some(cb) = state.callback.take() {
                        let mut url_errors = state
                            .result
                            .drain(..)
                            .filter_map(|(u, r)| match r {
                                Ok(_) => None,
                                Err(e) => Some((u, e)),
                            })
                            .collect::<Vec<_>>();
                        url_errors.sort_unstable_by_key(|(u, e)| u.to_owned());
                        let result = match url_errors.len() {
                            0 => Ok(()),
                            _ => {
                                let message = url_errors
                                    .iter()
                                    .map(|(u, e)| {
                                        if u.is_empty() {
                                            e.to_string()
                                        } else {
                                            format!("{}: {}", u, e)
                                        }
                                    })
                                    .collect::<Vec<_>>()
                                    .join("\n");
                                Err(io::Error::new(url_errors[0].1.kind(), message))
                            }
                        };
                        cb(result);
                    }
                }
            }))
        };

        for mount in table.mounts.iter_mut().rev() {
            if mount.url.is_some() {
                process(mount)
            }
        }
        process(&mut self.root);
    }
}

impl MultiplexBackend {
    pub fn from_root_backend(backend: BoxBackend) -> Self {
        let backend = Self {
            backends: vec![backend],
            table_srcdst: Default::default(),
            table_dstsrc: Default::default(),
        };
        backend
    }

    fn extract_url(&self, id: FullId) -> io::Result<Cow<str>> {
        self.extract_meta(id, "mount=")
    }

    pub fn mount(&mut self, id: FullId, backend: BoxBackend) -> Result<FullId> {
        if id.1 == 0 {
            return notebackend_types::error::invalid_input("cannot mount at root");
        }
        if self.table_srcdst.contains_key(&id) {
            return notebackend_types::error::invalid_input("already mounted");
        }
        let backend_id = self.backends.len();
        let root_id = backend.get_root_id();
        self.backends.push(backend);
        let mid = (backend_id, root_id);
        self.table_srcdst.insert(id, mid);
        self.table_dstsrc.insert(mid, id);
        self.touch(id)?;
        self.touch(mid)?;
        let url = self.extract_url(id)?;
        log::info!("mount {:?} => {:?} ({})", id, mid, url);
        Ok((backend_id, root_id))
    }

    pub fn umount(&mut self, id: FullId) -> Result<()> {
        if let Some(mid) = self.table_srcdst.get(&id).cloned() {
            self.backends[mid.0].persist()?;
            let url = self.extract_url(id)?;
            log::info!("umount {:?} => {:?} ({})", id, mid, url);
            self.touch(id)?;
            self.touch(mid)?;
            self.table_dstsrc.remove(&mid);
            self.table_srcdst.remove(&id);
            self.backends[mid.0] = Box::new(NullBackend);

            // Remove stale backends.
            if let Some(max_backend_id) = self.table_dstsrc.keys().map(|b| b.0).max() {
                let needed_len = max_backend_id + 1;
                let len = self.backends.len();
                if needed_len < len {
                    log::info!("resizing backends {} -> {:?}", len, needed_len);
                    self.backends.truncate(needed_len);
                }
            }
        }

        Ok(())
    }

    pub fn is_mount(&self, id: FullId) -> Result<bool> {
        Ok(self.table_srcdst.contains_key(&id))
    }

    fn get_backend<'a>(&self, index: BackendId) -> Result<&BoxBackend> {
        self.backends.get(index).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, "backend index out of bound")
        })
    }

    fn get_backend_mut(&mut self, index: BackendId) -> Result<&mut BoxBackend> {
        self.backends.get_mut(index).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, "backend index out of bound")
        })
    }

    fn swap_backend(&mut self, index: BackendId, backend: &mut BoxBackend) {
        let p = &mut self.backends[index];
        std::mem::swap(p, backend);
    }

    /// Bump mtime across mount boundary.
    fn bump_parent_mtime(&mut self, mut id: FullId) -> Result<()> {
        log::trace!("bumping mtime for {:?}", id);
        let mut last_backend_id = id.0;
        while let Some(parent) = self.get_parent(id)? {
            id = parent;
            if id.0 != last_backend_id {
                self.get_backend_mut(id.0)?.touch(id.1)?;
                last_backend_id = id.0
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::tests::*;
    use crate::backend::MemBackend;

    #[test]
    fn test_basic() {
        let root_backend = MemBackend::empty();
        let mut backend = MountableBackend::from_root_backend(Box::new(root_backend));
        backend.check_generic().unwrap();
    }

    #[test]
    fn test_mount() {
        let mut m1 = MemBackend::empty();
        m1.insert_ascii("A--B--C");

        let mut root = MountableBackend::from_root_backend(Box::new(m1));
        assert_eq!(
            root.draw_ascii(&root.find_ids("root A B C")),
            r#"
                root
                \_ 1 ("A")
                   \_ 2 ("B")
                      \_ 3 ("C")"#
        );

        root.update_meta(root.find("B"), "mount=", "memory:ascii=X--Y")
            .unwrap();
        assert_eq!(
            root.draw_ascii(&root.find_ids("root A B X Y")),
            r#"
                root
                \_ 1 ("A")
                   \_ 2 ("B")
                      \_ 3 ("X")
                         \_ 4 ("Y")"#
        );

        root.check_mtime_changed(root.find("A"), |b| {
            b.touch(b.find("Y")).unwrap();
        });

        root.check_parent(root.find("X"), root.find("B"));
        root.update_meta(root.find("B"), "mount=", "").unwrap();
        assert_eq!(
            root.draw_ascii(&root.find_ids("root A B C")),
            r#"
                root
                \_ 1 ("A")
                   \_ 2 ("B")
                      \_ 3 ("C")"#
        );
    }
}
