//! Rust library backend. Dynamically load backends at runtime can have other
//! dependencies.

use libloading::Library;
use libloading::Symbol;
use notebackend_types::TreeBackend;
use notebackend_types::{CreateBackendFunc, Id};
use std::io;
use std::sync::Arc;
use std::sync::Mutex;
pub struct DylibBackend {
    // The order here is important. `tree` needs to be dropped before `lib`.
    tree: Box<dyn TreeBackend<Id = Id>>,
    lib: Library,
}

impl DylibBackend {
    pub fn open(lib_name: &str, url: &str) -> io::Result<Self> {
        let lib =
            Library::new(lib_name).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
        // safety: the shared library should be built, shipped together with
        // this crate and has the matching interface.
        let create_func: Symbol<CreateBackendFunc> = unsafe {
            lib.get(b"notebackend_create")
                .map_err(|e| io::Error::new(io::ErrorKind::NotFound, e))?
        };
        let tree = create_func(url).map_err(|e| {
            // The error data might refer to static segments in the dylib.
            // They will become invalid after dropping "lib". So turn them
            // into plain strings first.
            io::Error::new(e.kind(), e.to_string())
        })?;
        Ok(Self { lib, tree })
    }
}

impl TreeBackend for DylibBackend {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> io::Result<Vec<Self::Id>> {
        self.tree.get_children(id)
    }

    fn get_parent(&self, id: Self::Id) -> io::Result<Option<Self::Id>> {
        self.tree.get_parent(id)
    }

    fn get_mtime(&self, id: Self::Id) -> io::Result<notebackend_types::Mtime> {
        self.tree.get_mtime(id)
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        self.tree.get_text(id)
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        self.tree.get_raw_meta(id)
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: notebackend_types::InsertPos,
        text: String,
        meta: String,
    ) -> io::Result<Self::Id> {
        self.tree.insert(dest_id, pos, text, meta)
    }

    fn set_parent(
        &mut self,
        id: Self::Id,
        dest_id: Self::Id,
        pos: notebackend_types::InsertPos,
    ) -> io::Result<Self::Id> {
        self.tree.set_parent(id, dest_id, pos)
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> io::Result<()> {
        self.tree.set_text(id, text)
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> io::Result<()> {
        self.tree.set_raw_meta(id, content)
    }

    fn remove(&mut self, id: Self::Id) -> io::Result<()> {
        self.tree.remove(id)
    }

    fn persist(&mut self) -> io::Result<()> {
        self.tree.persist()
    }

    fn persist_async(&mut self, result: Arc<Mutex<Option<io::Result<()>>>>) {
        self.tree.persist_async(result)
    }

    fn get_root_id(&self) -> Self::Id {
        self.tree.get_root_id()
    }

    fn is_ancestor(&self, ancestor: Self::Id, descendant: Self::Id) -> io::Result<bool> {
        self.tree.is_ancestor(ancestor, descendant)
    }

    fn extract_meta<'a>(
        &'a self,
        id: Self::Id,
        prefix: &str,
    ) -> io::Result<std::borrow::Cow<'a, str>> {
        self.tree.extract_meta(id, prefix)
    }

    fn update_meta(&mut self, id: Self::Id, prefix: &str, value: &str) -> io::Result<()> {
        self.tree.update_meta(id, prefix, value)
    }

    fn get_text_first_line(&self, id: Self::Id) -> io::Result<String> {
        self.tree.get_text_first_line(id)
    }

    fn set_parent_batch(
        &mut self,
        ids: &[Self::Id],
        dest_id: Self::Id,
        pos: notebackend_types::InsertPos,
    ) -> io::Result<Vec<Self::Id>> {
        self.tree.set_parent_batch(ids, dest_id, pos)
    }

    fn touch(&mut self, id: Self::Id) -> io::Result<()> {
        self.tree.touch(id)
    }

    fn remove_batch(&mut self, ids: &[Self::Id]) -> io::Result<()> {
        self.tree.remove_batch(ids)
    }

    fn autofill(&mut self, id: Self::Id) -> io::Result<()> {
        self.tree.autofill(id)
    }

    fn get_heads(&self, ids: &[Self::Id]) -> io::Result<Vec<Self::Id>> {
        self.tree.get_heads(ids)
    }
}
