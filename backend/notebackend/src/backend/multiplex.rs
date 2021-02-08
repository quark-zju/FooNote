//! Multiplex backend. Mixing multiple backends together.

use super::null::NullBackend;
use crate::clipboard;
use notebackend_types::meta::TreeMeta;
use notebackend_types::BackendId;
use notebackend_types::Id;
use notebackend_types::InsertPos;
use notebackend_types::Mtime;
use notebackend_types::TreeBackend;
use std::collections::HashMap;
use std::io;
use std::io::Result;
use std::sync::Arc;
use std::sync::Mutex;

pub type FullId = (BackendId, Id);
type BoxBackend = Box<dyn TreeBackend<Id = Id>>;

const ROOT_BACKEND_ID: BackendId = 0;

/// Multiplex backend. A backend node can mount other backend roots.
#[derive(Default)]
pub struct MultiplexBackend {
    /// Backends, referred by BackendId. The first one is the root backend.
    backends: Vec<BoxBackend>,

    /// Parents of backends.
    parents: Vec<FullId>,

    /// Mount overrides. Original Id -> New mounted Id.
    table_srcdst: HashMap<FullId, FullId>,

    /// Reverse of table. Useful to lookup parents.
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

    fn set_text(&mut self, id: Self::Id, text: String) -> Result<()> {
        let backend = self.get_backend_mut(id.0)?;
        backend.set_text(id.1, text)?;
        self.bump_parent_mtime(id)?;
        Ok(())
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> Result<()> {
        let backend = self.get_backend_mut(id.0)?;
        backend.set_raw_meta(id.1, content)?;
        self.bump_parent_mtime(id)?;
        Ok(())
    }

    fn touch(&mut self, id: Self::Id) -> Result<()> {
        let backend = self.get_backend_mut(id.0)?;
        backend.touch(id.1)?;
        self.bump_parent_mtime(id)?;
        Ok(())
    }

    fn remove(&mut self, id: Self::Id) -> Result<()> {
        // XXX: This can cause dangling mounts.
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

    fn persist_async(&mut self, result: Arc<Mutex<Option<Result<()>>>>) {
        // XXX: Fast path only works for the 1 backend case.
        if self.backends.len() == 1 {
            return self.backends[0].persist_async(result);
        }
        *result.lock().unwrap() = Some(self.persist());
    }
}

impl MultiplexBackend {
    pub fn from_root_backend(backend: BoxBackend) -> Self {
        Self {
            backends: vec![backend],
            parents: vec![(0, 0)],
            table_srcdst: Default::default(),
            table_dstsrc: Default::default(),
        }
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
        eprintln!("mounted!");
        self.touch(id)?;
        Ok((backend_id, root_id))
    }

    pub fn umount(&mut self, id: FullId) -> Result<()> {
        if let Some(fid) = self.table_srcdst.remove(&id) {
            self.table_dstsrc.remove(&fid);
            self.backends[fid.0] = Box::new(NullBackend);
            self.touch(id)?;
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
        let mut backend = MultiplexBackend::from_root_backend(Box::new(root_backend));
        backend.check_generic().unwrap();
    }

    #[test]
    fn test_mount() {
        let mut m1 = MemBackend::empty();
        let mut m2 = MemBackend::empty();
        m1.insert_ascii("A--B--C");
        m2.insert_ascii("X--Y");

        let mut root = MultiplexBackend::from_root_backend(Box::new(m1));
        assert_eq!(
            root.draw_ascii(&root.find_ids("root A B C")),
            r#"
                root
                \_ 1 ("A")
                   \_ 2 ("B")
                      \_ 3 ("C")"#
        );

        root.mount(root.find("B"), Box::new(m2)).unwrap();
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
        root.umount(root.find("B")).unwrap();
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
