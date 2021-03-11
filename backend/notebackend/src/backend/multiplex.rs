//! Multiplex backend. Mixing multiple backends together.

use super::null::NullBackend;
use crate::clipboard;
use crate::t;
use crate::url::BackendType;
use notebackend_types::log;
use notebackend_types::meta::TreeMeta;
use notebackend_types::BackendId;
use notebackend_types::Id;
use notebackend_types::InsertPos;
use notebackend_types::Mtime;
use notebackend_types::PersistCallbackFunc;
use notebackend_types::TreeBackend;
use parking_lot::Mutex;
use parking_lot::RwLock;
use parking_lot::RwLockUpgradableReadGuard;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io;
use std::io::Result;
use std::sync::Arc;

pub type FullId = (BackendId, Id);
type BoxBackend = Box<dyn TreeBackend<Id = Id>>;

const ROOT_BACKEND_ID: BackendId = 0;

/// Based on another backend. Treat "mount=" nodes specially by "mounting"
/// other backends into those nodes.
pub struct MultiplexBackend {
    root: Mount,
    table: RwLock<MountTable>,

    // Passwords used for mounting encrypted nodes.
    passwords: HashMap<String, String>,
}

#[derive(Default)]
struct MountTable {
    /// Backends, referred by BackendId. The first one is the root backend.
    mounts: Vec<Mount>,

    /// Url to backend Id.
    key_to_id: HashMap<String, BackendId>,
}

struct Mount {
    backend: BoxBackend,
    error_message: Option<String>,

    /// The node that has the "mount=url" meta.
    source_id: Vec<FullId>,
    url: Option<String>,
    key: Option<String>,

    /// Whether this mount is "inlined".
    /// An inlined mount does not save to the "url", but inlined
    /// in its mount source.
    inline: bool,
}

impl Default for MultiplexBackend {
    fn default() -> Self {
        Self::from_root_backend(Box::new(NullBackend))
    }
}

impl MultiplexBackend {
    pub fn open_url(url: &str) -> io::Result<Self> {
        let backend = crate::url::open(url, None)?;
        let result = Self {
            root: Mount {
                backend,
                error_message: None,
                source_id: Vec::new(),
                url: Some(url.to_string()),
                key: None,
                inline: false,
            },
            table: Default::default(),
            passwords: Default::default(),
        };
        Ok(result)
    }

    pub fn from_root_backend(backend: BoxBackend) -> Self {
        Self {
            root: Mount {
                backend,
                error_message: None,
                source_id: Vec::new(),
                url: None,
                key: None,
                inline: false,
            },
            table: Default::default(),
            passwords: Default::default(),
        }
    }

    /// Obtain information about mount URL, key, encrypted, inline.
    fn mount_url_key_encrypted_inline(
        &self,
        id: FullId,
    ) -> Result<Option<(Cow<str>, String, bool, bool)>> {
        // Root id cannot be mounted.
        if id == self.get_root_id() {
            return Ok(None);
        }

        let url = self.extract_url(id)?;
        if url.is_empty() {
            // No URL to mount.
            return Ok(None);
        }

        // The aes256 backend is the only one that is inlined.
        let encrypted = self.is_encrytped_mount(id)?;
        let inline = self.is_inlined_mount(id)?;
        if encrypted {
            assert!(inline, "encrypted = true should indicate inline = true")
        }

        #[cfg(any())]
        let key = if inline {
            // Make "key" unique per node (not a good idea?)
            format!("i:{:?}:{}", id, url)
        } else {
            format!("u:{}", url)
        };

        let key = url.to_string();
        Ok(Some((url, key, encrypted, inline)))
    }

    /// Test if a node is mounted without errors.
    fn is_mounted(&self, id: FullId) -> Result<bool> {
        let (url, key, encrypted, inline) = match self.mount_url_key_encrypted_inline(id)? {
            Some(v) => v,
            None => return Ok(false),
        };
        let table = self.table.read();
        if let Some(&backend_index) = table.key_to_id.get(&key) {
            let mount = &table.mounts[backend_index];
            Ok(mount.error_message.is_none() && mount.source_id.contains(&id))
        } else {
            Ok(false)
        }
    }

    /// Mount `id` on demand. Return the mounted id.
    /// If `auto_mount` is false, then avoid mounting new backends.
    fn follow_mount(&self, id: FullId, auto_mount: bool) -> io::Result<FullId> {
        let (url, key, encrypted, mut inline) = match self.mount_url_key_encrypted_inline(id)? {
            Some(v) => v,
            None => return Ok(id),
        };

        {
            let mut table = self.table.write();
            if let Some(&backend_index) = table.key_to_id.get(&key) {
                // Already mounted.
                log::trace!("Reuse backend {} for URL {}", backend_index, &url,);
                let mount = &mut table.mounts[backend_index];
                let root_id = mount.backend.get_root_id();
                if !mount.source_id.contains(&id) {
                    mount.source_id.push(id)
                }
                let backend_id = backend_index + 1;
                return Ok((backend_id, root_id));
            }
        }

        if !auto_mount {
            log::trace!("Skipping auto-mount {} at {:?}", &url, id);
            return Ok(id);
        }

        log::debug!("Attempt to mount {} at {:?} (inline={})", &url, id, inline);
        let mut error_message = None;
        let inline_data = if inline {
            // Pick the text. Note this might need read (lock) access on table.
            let text = self.get_text(id)?;
            let data = decode_inlined_last_line(&text)?;
            Some(data)
        } else {
            None
        };
        let backend = if encrypted {
            // Set via "update_meta".
            let password = self.passwords.get(&url.to_string());
            match password {
                Some(password) => crate::url::open_aes256(password, inline_data.unwrap()),
                None => Err(io::Error::new(
                    io::ErrorKind::PermissionDenied,
                    t!(cn = "需要密码", en = "Password Required"),
                )),
            }
        } else {
            crate::url::open(url.as_ref(), inline_data.as_deref())
        };
        let backend = match backend {
            Ok(backend) => backend,
            Err(e) => {
                log::warn!("Failed to mount {}: {:?}", &url, e);
                error_message = Some(e.to_string());
                inline = false;
                warning_backend(e.to_string())
            }
        };
        let root_id = backend.get_root_id();

        // Find a free entry (url is empty, meaning umount-ed).
        let mut table = self.table.write();
        let backend_index = table
            .mounts
            .iter()
            .enumerate()
            .filter_map(|(i, m)| if m.url.is_none() { Some(i) } else { None })
            .next();
        let mount = Mount {
            backend,
            error_message,
            source_id: vec![id],
            url: Some(url.to_string()),
            key: Some(key.clone()),
            inline,
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
        table.key_to_id.insert(key, backend_index);

        let backend_id = backend_index + 1;
        log::info!("Mounted {} for {:?} as backend {}", &url, id, backend_id);

        Ok((backend_id, root_id))
    }

    /// Unmount descendants of `id`.
    /// If `all` is true, all nodes of the same url will be umounted.
    fn umount_recursive(&mut self, id: FullId, all: bool) -> io::Result<()> {
        // Persist inline changes to avoid data loss.
        // XXX: This probably needs to be recursive? And if inlined backends
        // are recursive then what to do?
        self.save_inlined_backends(Some(id))?;

        log::debug!("umount_recursive {:?} (all={})", id, all);
        let table = self.table.upgradable_read();

        // Find out what to persist, and what source_ids to update.
        let mut to_umount_index = Vec::new();
        let mut source_ids_vec = table
            .mounts
            .iter()
            .map(|m| m.source_id.clone())
            .collect::<Vec<_>>();
        for (i, source_ids) in source_ids_vec.iter_mut().enumerate() {
            if all && source_ids.contains(&id) {
                source_ids.clear();
            } else {
                *source_ids = source_ids
                    .drain(..)
                    .filter(|&src_id| self.is_ancestor(id, src_id).ok() != Some(true))
                    .collect();
            }
            if source_ids.is_empty() {
                to_umount_index.push(i);
            }
        }

        let mut table = RwLockUpgradableReadGuard::upgrade(table);

        // Persist backend to avoid data loss.
        for i in to_umount_index.into_iter().rev() {
            let mount = &mut table.mounts[i];
            let url = match mount.url.as_ref() {
                Some(url) => url.clone(),
                None => continue,
            };
            // Attempt to avoid losing unsaved content.
            log::trace!(" saving backend {} ({})", i + 1, &url);
            mount.backend.persist()?;
            mount.backend = Box::new(NullBackend);
            mount.url = None;
            mount.error_message = Some("umounted".to_string());
            let key = match mount.key.as_ref() {
                Some(key) => key.clone(),
                None => continue,
            };
            table.key_to_id.remove(&key);
            log::info!("umounted backend {} ({})", i + 1, &url);
        }

        // Update source_ids after persist().
        for (i, source_ids) in source_ids_vec.into_iter().enumerate() {
            let mount = &mut table.mounts[i];
            if mount.source_id != source_ids {
                log::trace!(
                    " backend {} source {:?} => {:?}",
                    i + 1,
                    &mount.source_id,
                    &source_ids
                );
                table.mounts[i].source_id = source_ids;
            }
        }

        log::trace!("umount_recursive {:?} done", id);

        Ok(())
    }

    fn with_mount<R>(
        &self,
        id: FullId,
        func: impl FnOnce(&Mount, Id) -> io::Result<R>,
    ) -> io::Result<R> {
        let (backend_id, id) = id;
        if backend_id == ROOT_BACKEND_ID {
            // Root backend - no need to lock.
            func(&self.root, id)
        } else {
            let table = self.table.read();
            if let Some(mount) = table.mounts.get(backend_id - 1) {
                func(mount, id)
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    t!(
                        cn = "后端 {} 未挂载",
                        en = "backend {} is out of bound",
                        backend_id
                    ),
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
        if backend_id == ROOT_BACKEND_ID {
            // Root backend - no need to lock.
            func(&mut self.root, id)
        } else {
            let mut table = self.table.write();
            if let Some(mount) = table.mounts.get_mut(backend_id - 1) {
                func(mount, id)
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    t!(
                        cn = "后端 {} 未挂载",
                        en = "backend {} is out of bound",
                        backend_id
                    ),
                ));
            }
        }
    }

    /// Swap out the backend with the given backend_id. This is to make
    /// borrowck happy in `set_parent`.
    /// Be sure to call this function again to swap back the backend!
    fn swap_backend(&mut self, backend_id: BackendId, mut backend: BoxBackend) -> BoxBackend {
        use std::mem::swap;
        if backend_id == ROOT_BACKEND_ID {
            swap(&mut backend, &mut self.root.backend);
        } else {
            let mut table = self.table.write();
            let backend_index = backend_id - 1;
            if let Some(m) = table.mounts.get_mut(backend_index) {
                std::mem::swap(&mut backend, &mut m.backend);
            }
        }
        backend
    }

    fn extract_url(&self, id: FullId) -> io::Result<Cow<str>> {
        self.extract_meta(id, "mount=")
    }

    fn is_inlined_mount(&self, id: FullId) -> io::Result<bool> {
        Ok(self.is_encrytped_mount(id)? || !self.extract_meta(id, "inline=")?.is_empty())
    }

    fn is_encrytped_mount(&self, id: FullId) -> io::Result<bool> {
        let url = self.extract_url(id)?;
        Ok(crate::url::backend_type_from_url(&url)? == BackendType::Aes256)
    }

    fn get_children_follow_mounts(&self, id: FullId, auto_mount: bool) -> Result<Vec<FullId>> {
        let fid = self.follow_mount(id, auto_mount)?;
        let children = self
            .with_mount(fid, |m, id| m.backend.get_children(id))?
            .into_iter()
            .map(|i| (fid.0, i))
            .collect();
        Ok(children)
    }

    /// Save inlined backends. If `source_id` is None, save all to text.
    /// Otherwise only save backends with the given `source_id`.
    fn save_inlined_backends(&mut self, source_id: Option<FullId>) -> Result<()> {
        let mut table = self.table.write();
        let mut to_write = Vec::new();

        // Save all inlined backends first.
        for mount in table.mounts.iter_mut() {
            if !mount.inline || mount.error_message.is_some() {
                continue;
            }
            if let Some(id) = source_id {
                if !mount.source_id.contains(&id) {
                    continue;
                }
            }

            mount.backend.persist()?;

            let data = mount.backend.inline_data().unwrap_or(b"");
            log::debug!(
                "write inline data ({:?} bytes) to {:?}",
                &data.len(),
                &mount.source_id
            );
            to_write.push((data.to_vec(), mount.source_id.clone()));
        }

        drop(table);

        for (data, source_ids) in to_write {
            let encoded = encode_base64(&data);
            for id in source_ids {
                let text = self.get_text(id).unwrap_or_default();
                let text = replace_last_line(&text, &encoded);
                self.set_text(id, text)?;
            }
        }

        Ok(())
    }
}

impl TreeBackend for MultiplexBackend {
    type Id = FullId;

    fn get_root_id(&self) -> Self::Id {
        (ROOT_BACKEND_ID, self.root.backend.get_root_id())
    }

    fn get_children(&self, id: Self::Id) -> Result<Vec<Self::Id>> {
        self.get_children_follow_mounts(id, true)
    }

    fn get_parent(&self, id: Self::Id) -> Result<Option<Self::Id>> {
        let backend_id = id.0;
        self.with_mount(id, |m, id| match m.backend.get_parent(id)? {
            Some(id) => {
                if id == m.backend.get_root_id() {
                    // Rewrite root id into the non-root id in source.
                    if let Some(id) = m.source_id.get(0).cloned() {
                        return Ok(Some(id));
                    }
                }
                Ok(Some((backend_id, id)))
            }
            None => {
                if id == m.backend.get_root_id() {
                    // Across mount boundary.
                    if let Some(&fid) = m.source_id.get(0) {
                        if backend_id != fid.0 {
                            return self.get_parent(fid);
                        }
                    }
                }
                Ok(None)
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
        let dest_id = self.follow_mount(dest_id, true)?;
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
        let dest_id = self.follow_mount(dest_id, pos == InsertPos::Append)?;
        if src_id.0 == dest_id.0 {
            // Move within a single backend.
            log::debug!("move {:?} to {:?} {:?}", src_id, pos, dest_id);
            self.touch(src_id)?;
            let moved_id = self.with_mount_mut(dest_id, |m, dest_id| {
                m.backend.set_parent(src_id.1, dest_id, pos)
            })?;
            self.touch(src_id)?;
            Ok((src_id.0, moved_id))
        } else {
            // Sanity check.
            let parent_id = match pos {
                InsertPos::Before | InsertPos::After => self
                    .get_parent(dest_id)?
                    .unwrap_or_else(|| self.get_root_id()),
                InsertPos::Append => dest_id,
            };
            if self.is_ancestor(src_id, parent_id)? {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    t!(
                        cn = "无法将 {:?} ({:?}) 移动到其子节点 {:?} ({:?})",
                        en = "{:?} ({:?}) cannot be moved to be under its descendant {:?} ({:?})",
                        self.get_text_first_line(src_id).unwrap_or_default(),
                        src_id,
                        self.get_text_first_line(parent_id).unwrap_or_default(),
                        dest_id,
                    ),
                ));
            }

            // Do the move.
            log::debug!("cross-move {:?} to {:?} {:?}", src_id, pos, dest_id);
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
                Ok((dest_id.0, dst_id))
            });

            // Swap back.
            self.swap_backend(src_id.0, src);
            if let Ok(new_id) = moved_id_result {
                self.touch(new_id)?;
            }

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
        log::debug!("remove {:?}", id);
        self.with_mount_mut(id, |m, local_id| m.backend.remove(local_id))?;
        self.touch(id)?;
        if !self.is_ancestor(self.get_root_id(), id)? {
            log::trace!("{:?} is no longer reachable from root", id);
            if self.is_mounted(id)? {
                self.umount_recursive(id, false)?;
            }
        }
        Ok(())
    }

    /// Bump mtime across mount boundary.
    fn touch(&mut self, id: FullId) -> Result<()> {
        log::debug!("touch {:?}", id);
        if id.0 == ROOT_BACKEND_ID {
            // Fast path.
            log::trace!(" touch root backend {:?}", id.1);
            self.root.backend.touch(id.1)?;
            return Ok(());
        }

        // Handle multi-parents.
        let mut touched = HashSet::new();
        let mut to_touch = vec![id];

        let mut table = self.table.write();
        while let Some(id) = to_touch.pop() {
            if !touched.insert(id) {
                log::trace!(" touched {:?}", id);
                continue;
            }
            if id.0 == ROOT_BACKEND_ID {
                log::trace!(" touch root backend {}", id.1);
                self.root.backend.touch(id.1)?;
                continue;
            }
            let backend_id = id.0;
            let backend_index = backend_id - 1;
            if let Some(mount) = table.mounts.get_mut(backend_index) {
                log::trace!(" touch backend {} {}", backend_id, id.1);
                mount.backend.touch(id.1)?;
                log::trace!(" touch extend {:?}", &mount.source_id);
                to_touch.extend_from_slice(&mount.source_id);
            }
        }
        log::trace!("touch {:?} done", id);

        Ok(())
    }

    fn persist(&mut self) -> Result<()> {
        self.save_inlined_backends(None)?;
        let mut table = self.table.write();
        for mount in table.mounts.iter_mut().rev() {
            mount.backend.persist()?;
        }
        self.root.backend.persist()?;
        Ok(())
    }

    fn persist_async(&mut self, callback: PersistCallbackFunc) {
        // Save all inlined backends first.
        if let Err(e) = self.save_inlined_backends(None) {
            log::warn!("Failed to save inlined backends: {}", &e);
            callback(Err(e));
            return;
        }

        let mut table = self.table.write();
        struct State {
            callback: Option<PersistCallbackFunc>,
            result: Vec<(String, io::Result<()>)>,
            waiting_count: usize,
        }
        let state = Arc::new(Mutex::new(State {
            callback: Some(callback),
            result: Default::default(),
            // Must match the call count of "process" below.
            waiting_count: 1 + table
                .mounts
                .iter()
                // The filter condition MUST match the "for" loop below.
                .filter(|m| m.url.is_some() && !m.inline)
                .count(),
        }));

        let process = |mount: &mut Mount| {
            let state = state.clone();
            let url = mount.url.clone().unwrap_or_else(|| "".to_string());
            mount.backend.persist_async(Box::new(move |r| {
                let mut state = state.lock();
                state.waiting_count -= 1;
                log::debug!("persist: {}: {:?}, {} left", &url, &r, state.waiting_count);
                state.result.push((url, r));
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
            // The if condition MUST match the filter condition above.
            if mount.url.is_some() && !mount.inline {
                process(mount)
            }
        }
        process(&mut self.root);
    }

    fn set_parent_batch(
        &mut self,
        ids: &[Self::Id],
        mut dest_id: Self::Id,
        mut pos: InsertPos,
    ) -> Result<Vec<Self::Id>> {
        log::info!("set_parent_batch {:?} => {:?} {:?}", ids, pos, dest_id);
        let heads = self.get_heads(ids)?;
        let mut new_ids = Vec::with_capacity(heads.len());
        for id in heads {
            let new_id = self.set_parent(id, dest_id, pos)?;
            new_ids.push(new_id);
            dest_id = new_id;
            pos = InsertPos::After;
        }
        Ok(new_ids)
    }

    fn remove_batch(&mut self, ids: &[Self::Id]) -> Result<()> {
        log::debug!("remove_batch {:?}", ids);
        let heads = self.get_heads(ids)?;
        log::trace!("remove_batch {:?}", &heads);
        for id in heads {
            self.remove(id)?;
        }
        log::trace!("remove_batch - done");
        Ok(())
    }

    fn inline_data(&self) -> Option<&[u8]> {
        None
    }

    fn update_meta(&mut self, id: Self::Id, prefix: &str, value: &str) -> Result<bool> {
        if prefix == "password=" {
            // Attempt to re-mount.
            let url = self.extract_url(id)?.to_string();
            if self.is_encrytped_mount(id)? && !url.is_empty() {
                log::debug!("Update meta attempts to remount {:?}", id);
                self.umount_recursive(id, true)?;
                if value.is_empty() {
                    // Umount. Forget password.
                    self.passwords.remove(&url);
                } else {
                    self.passwords.insert(url, value.to_string());
                    // Remount with the new password.
                    let _ = self.follow_mount(id, true);
                }
                self.touch(id)?;
            }
            Ok(true)
        } else {
            notebackend_types::tree::update_meta(self, id, prefix, value)
        }
    }

    fn extract_meta<'a>(&'a self, id: Self::Id, prefix: &str) -> Result<Cow<'a, str>> {
        if prefix == "mounted=" {
            // Special case: test if a node is mounted.
            if self.is_mounted(id)? {
                Ok("true".into())
            } else {
                Ok("".into())
            }
        } else {
            notebackend_types::tree::extract_meta(self, id, prefix)
        }
    }
}

/// Dummy backend for warning purpose
fn warning_backend(message: String) -> BoxBackend {
    let len = message.len();
    let mut backend = crate::backend::MemBackend::empty();
    let id = backend
        .insert(
            backend.get_root_id(),
            InsertPos::Append,
            message,
            "type=warn\nreadonly=true\ncopyable=false\nmovable=false\n".into(),
        )
        .expect("insert to MemBackend should succeed");
    // HACK: Try to update the mtime of backend text.
    // So reusing a backend (ex. switching from warning to aes) can trigger
    // refreshing correctly.
    for _ in 3..(len % 32) {
        let _ = backend.touch(id);
    }
    Box::new(backend.freeze())
}

fn replace_last_line(text: &str, line: &str) -> String {
    let mut lines: Vec<&str> = text.lines().collect();
    match lines.last_mut() {
        Some(last) if last.starts_with("base64:") => {
            *last = line;
        }
        _ => {
            lines.push(line);
        }
    }
    lines.join("\n")
}

fn encode_base64(data: &[u8]) -> String {
    format!("base64:{}", base64::encode(data))
}

fn decode_inlined_last_line(text: &str) -> Result<Vec<u8>> {
    let prefix = "base64:";
    let last_line = text.lines().filter(|l| l.starts_with(prefix)).last();
    match last_line {
        None => Ok(Vec::new()),
        Some(s) => base64::decode(&s[prefix.len()..])
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e)),
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
        let mut backend = MultiplexBackend::from_root_backend(Box::new(root_backend));
        backend.check_generic().unwrap();
    }

    #[test]
    fn test_mount() {
        let mut m1 = MemBackend::empty();
        m1.insert_ascii("A--B--C");

        let mut root = MultiplexBackend::from_root_backend(Box::new(m1));
        assert_eq!(
            root.draw_ascii(&root.find_ids("root A B C")),
            r#"
                root
                \_ 1 ("A")
                   \_ 2 ("B")
                      \_ 3 ("C")"#
        );
        assert!(!root.is_mounted(root.get_root_id()).unwrap());
        assert!(!root.is_mounted(root.find("A")).unwrap());
        assert!(!root.is_mounted(root.find("B")).unwrap());
        assert!(!root.is_mounted(root.find("C")).unwrap());

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
        assert!(root.is_mounted(root.find("B")).unwrap());
        assert_eq!(
            root.extract_meta(root.find("B"), "mounted=").unwrap(),
            "true"
        );
        assert_eq!(root.extract_meta(root.find("A"), "mounted=").unwrap(), "");

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

    #[test]
    fn test_cross_mount_move() {
        let mut root = MultiplexBackend::open_url("memory").unwrap();
        root.insert_ascii(
            r#"
                A--B
                 \
                  C"#,
        );
        root.update_meta(root.find("B"), "mount=", "memory:1")
            .unwrap();
        root.update_meta(root.find("C"), "mount=", "memory:2")
            .unwrap();

        let b1 = root.quick_insert(root.find("B"), "B1");
        root.quick_insert(root.find("B1"), "B2");
        root.quick_insert(b1, "B3");
        root.quick_insert(root.find("C"), "C1");

        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("A")
                   \_ 4 ("B")
                   |  \_ 5 ("B1")
                   |     \_ 7 ("B2")
                   |     \_ 6 ("B3")
                   \_ 2 ("C")
                      \_ 3 ("C1")"#
        );

        // Move B1 (and B2) to C1
        root.check_mtime_changed(root.find("C"), |root| {
            root.set_parent(root.find("B1"), root.find("C1"), InsertPos::After)
                .unwrap();
        });
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("A")
                   \_ 7 ("B")
                   \_ 2 ("C")
                      \_ 6 ("C1")
                      \_ 3 ("B1")
                         \_ 5 ("B2")
                         \_ 4 ("B3")"#
        );
    }

    #[test]
    fn test_duplicated_mount() {
        let mut root = MultiplexBackend::open_url("memory").unwrap();
        let mut insert = |t: &str| {
            root.insert(
                root.get_root_id(),
                InsertPos::Append,
                t.into(),
                "mount=memory:a\n".into(),
            )
            .unwrap();
        };
        insert("A");
        insert("B");
        root.quick_insert(root.find("A"), "C");
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 3 ("A")
                |  \_ 2 ("C")
                \_ 1 ("B")
                   \_ 2 ("C")"#
        );
        let p = root.get_parent(root.find("C")).unwrap();
        assert_eq!(root.find("A"), p.unwrap());
    }

    #[test]
    fn test_inline_mount() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("a.foonote");
        let backend = crate::backend::SingleFileBackend::from_path(&path).unwrap();
        let mut root = MultiplexBackend::from_root_backend(Box::new(backend));
        let id = root
            .insert(
                root.get_root_id(),
                InsertPos::Append,
                "Inlined memory\n".to_string(),
                "mount=memory\ninline=true\n".into(),
            )
            .unwrap();
        root.insert(id, InsertPos::Append, "foo".to_string(), Default::default())
            .unwrap();
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Inlined memory")
                   \_ 2 ("foo")"#
        );
        root.persist().unwrap();
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Inlined memory")
                   \_ 2 ("foo")"#
        );
        drop(root);

        // Reload
        let backend = crate::backend::SingleFileBackend::from_path(&path).unwrap();
        let mut root = MultiplexBackend::from_root_backend(Box::new(backend));
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Inlined memory")
                   \_ 2 ("foo")"#
        );
        assert_eq!(
            root.get_text(id).unwrap(),
            "Inlined memory\nbase64:omF0oQpjZm9vYW2jYWOhAIEKYW2hAGp0eXBlPXJvb3QKYW4L"
        );

        // Change again
        root.insert(id, InsertPos::Append, "bar".to_string(), Default::default())
            .unwrap();
        assert_eq!(
            root.get_text(id).unwrap(),
            "Inlined memory\nbase64:omF0oQpjZm9vYW2jYWOhAIEKYW2hAGp0eXBlPXJvb3QKYW4L"
        );
        root.persist().unwrap();
        assert_eq!(
            root.get_text(id).unwrap(),
            r#"Inlined memory
base64:omF0ogpjZm9vC2NiYXJhbaNhY6EAggoLYW2hAGp0eXBlPXJvb3QKYW4M"#
        );
        drop(root);

        // Reload again
        let backend = crate::backend::SingleFileBackend::from_path(&path).unwrap();
        let root = MultiplexBackend::from_root_backend(Box::new(backend));
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Inlined memory")
                   \_ 3 ("foo")
                   \_ 2 ("bar")"#
        );
    }

    #[test]
    fn test_aes256() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("a.foonote");
        let backend = crate::backend::SingleFileBackend::from_path(&path).unwrap();
        let mut root = MultiplexBackend::from_root_backend(Box::new(backend));
        let id = root
            .insert(
                root.get_root_id(),
                InsertPos::Append,
                "Encrypted Node\n".to_string(),
                "mount=aes256\n".into(),
            )
            .unwrap();
        assert!(!root.is_mounted(id).unwrap());
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Encrypted Node")
                   \_ 2 ("Password Required") (type=warn)"#
        );
        // Should not crash persisting with the error.
        root.persist().unwrap();

        // Provide a password. Now insertion is supported.
        root.update_meta(id, "password=", "abc").unwrap();
        assert!(root.is_mounted(id).unwrap());
        root.quick_insert(id, "foo");
        let bar_id = root.quick_insert(id, "bar");
        root.set_text(bar_id, "bar2".to_string()).unwrap();
        root.persist().unwrap();
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Encrypted Node")
                   \_ 3 ("foo")
                   \_ 2 ("bar2")"#
        );

        // Reload.
        drop(root);
        let backend = crate::backend::SingleFileBackend::from_path(&path).unwrap();
        let mut root = MultiplexBackend::from_root_backend(Box::new(backend));
        assert!(!root.is_mounted(id).unwrap());
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Encrypted Node")
                   \_ 2 ("Password Required") (type=warn)"#
        );

        // Wrong password.
        root.update_meta(id, "password=", "def").unwrap();
        assert!(!root.is_mounted(id).unwrap());
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Encrypted Node")
                   \_ 2 ("Cannot decrypt: aead::Error") (type=warn)"#
        );

        // Correct password.
        root.update_meta(id, "password=", "abc").unwrap();
        assert!(root.is_mounted(id).unwrap());
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Encrypted Node")
                   \_ 3 ("foo")
                   \_ 2 ("bar2")"#
        );

        // "Lock" / "Close"
        root.update_meta(id, "password=", "").unwrap();
        assert!(!root.is_mounted(id).unwrap());
        assert_eq!(
            root.draw_ascii(&root.all_ids()),
            r#"
                root
                \_ 1 ("Encrypted Node")
                   \_ 2 ("Password Required") (type=warn)"#
        );
    }

    #[test]
    fn test_aes256_inside_other_mount() {
        let mut root = MultiplexBackend::open_url("memory").unwrap();
        let id = root.quick_insert(root.get_root_id(), "a");
        root.update_meta(id, "mount=", "memory").unwrap();
        let id2 = root.quick_insert(id, "a");
        root.update_meta(id2, "mount=", "aes256").unwrap();
        root.update_meta(id2, "password=", "a").unwrap();
        assert!(root.get_children(id2).unwrap().is_empty());
    }
}
