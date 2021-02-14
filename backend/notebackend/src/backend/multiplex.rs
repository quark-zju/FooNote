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
    source_id: Vec<FullId>,
    url: Option<String>,
}

impl Default for MultiplexBackend {
    fn default() -> Self {
        Self::from_root_backend(Box::new(NullBackend))
    }
}

impl MultiplexBackend {
    pub fn open_url(url: &str) -> io::Result<Self> {
        let backend = crate::url::open(url)?;
        let result = Self {
            root: Mount {
                backend,
                error_message: None,
                source_id: Vec::new(),
                url: Some(url.to_string()),
            },
            table: Default::default(),
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
            },
            table: Default::default(),
        }
    }

    /// Mount `id` on demand. Return the mounted id.
    /// If `auto_mount` is false, then avoid mounting new backends.
    fn follow_mount(&self, id: FullId, auto_mount: bool) -> io::Result<FullId> {
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
            let mount = &mut table.mounts[backend_index];
            let root_id = mount.backend.get_root_id();
            if !mount.source_id.contains(&id) {
                mount.source_id.push(id)
            }
            let backend_id = backend_index + 1;
            return Ok((backend_id, root_id));
        }

        if !auto_mount {
            log::trace!("Skipping auto-mount {} at {:?}", &url, id);
            return Ok(id);
        }

        log::debug!("Attempt to mount {} at {:?}", &url, id);
        let mut error_message = None;
        let backend = match crate::url::open(url.as_ref()) {
            Ok(backend) => backend,
            Err(e) => {
                log::warn!("Failed to mount {}: {:?}", &url, e);
                error_message = Some(e.to_string());
                warning_backend(e.to_string())
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
            source_id: vec![id],
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
        for (i, mount) in table.mounts.iter_mut().enumerate() {
            mount.source_id = mount
                .source_id
                .drain(..)
                .filter(|&src_id| self.is_ancestor(id, src_id).ok() == Some(true))
                .collect();
            if mount.source_id.is_empty() {
                to_umount_index.push(i);
            }
        }

        for i in to_umount_index.into_iter().rev() {
            let mount = &mut table.mounts[i];
            let url = match mount.url.as_ref() {
                Some(url) => url.clone(),
                None => continue,
            };
            // Attempt to avoid losing unsaved content.
            mount.backend.persist()?;
            mount.backend = Box::new(NullBackend);
            mount.url = None;
            mount.error_message = Some("umounted".to_string());
            table.url_to_id.remove(&url);
            log::info!("Umounted backend {} ({})", i + 1, &url);
        }

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

    fn get_children_ex(&self, id: FullId, auto_mount: bool) -> Result<Vec<FullId>> {
        let fid = self.follow_mount(id, auto_mount)?;
        let children = self
            .with_mount(fid, |m, id| m.backend.get_children(id))?
            .into_iter()
            .map(|i| (fid.0, i))
            .collect();
        Ok(children)
    }
}

impl TreeBackend for MultiplexBackend {
    type Id = FullId;

    fn get_root_id(&self) -> Self::Id {
        (ROOT_BACKEND_ID, self.root.backend.get_root_id())
    }

    fn get_children(&self, id: Self::Id) -> Result<Vec<Self::Id>> {
        self.get_children_ex(id, true)
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
                Ok((dest_id.0, local_dest_id))
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
        self.with_mount_mut(id, |m, local_id| m.backend.remove(local_id))?;
        self.touch(id)?;
        if !self.is_ancestor(self.get_root_id(), id)? {
            log::trace!("{:?} is no longer reachable from root", id);
            self.umount_recursive(id)?;
        }
        Ok(())
    }

    /// Bump mtime across mount boundary.
    fn touch(&mut self, id: FullId) -> Result<()> {
        log::debug!("touch {:?}", id);
        if id.0 == ROOT_BACKEND_ID {
            // Fast path.
            self.root.backend.touch(id.1)?;
            return Ok(());
        }

        // Handle multi-parents.
        let mut touched = HashSet::new();
        let mut to_touch = vec![id];

        let mut table = self.table.write();
        while let Some(id) = to_touch.pop() {
            if !touched.insert(id) {
                continue;
            }
            if id.0 == ROOT_BACKEND_ID {
                self.root.backend.touch(id.1)?;
                continue;
            }
            if let Some(mount) = table.mounts.get_mut(id.0 - 1) {
                mount.backend.touch(id.1)?;
                to_touch.extend_from_slice(&mount.source_id);
            }
        }

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
            if mount.url.is_some() {
                process(mount)
            }
        }
        process(&mut self.root);
    }

    fn get_heads(&self, ids: &[Self::Id]) -> Result<Vec<Self::Id>> {
        let id_set: HashSet<Self::Id> = ids.iter().cloned().collect();
        let mut pushed = HashSet::new();
        let mut result = Vec::new();
        // Visit from the root (to preserve order).
        let mut to_visit = vec![self.get_root_id()];
        while let Some(id) = to_visit.pop() {
            if id_set.contains(&id) {
                if pushed.insert(id) {
                    result.push(id);
                }
            } else {
                // Visit children in order.
                // Avoid auto-mount in get_heads.
                let mut children = self.get_children_ex(id, false)?;
                children.reverse();
                to_visit.extend(children);
            }
        }
        log::debug!("get_heads({:?}) = {:?}", ids, &result);
        Ok(result)
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
}

/// Dummy backend for warning purpose
fn warning_backend(message: String) -> BoxBackend {
    let mut backend = crate::backend::MemBackend::empty();
    backend
        .insert(
            backend.get_root_id(),
            InsertPos::Append,
            message,
            "type=warn\nreadonly=true\ncopyable=false\nmovable=false\n".into(),
        )
        .expect("insert to MemBackend should succeed");
    Box::new(backend.freeze())
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
}
