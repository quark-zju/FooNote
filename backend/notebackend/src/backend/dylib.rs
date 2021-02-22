//! Rust library backend. Dynamically load backends at runtime can have other
//! dependencies.

use libloading::Library;
use libloading::Symbol;
use notebackend_types::CreateBackendFunc;
use notebackend_types::Id;
use notebackend_types::PersistCallbackFunc;
use notebackend_types::TreeBackend;
use std::io;
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

    fn set_text(&mut self, id: Self::Id, text: String) -> io::Result<bool> {
        self.tree.set_text(id, text)
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> io::Result<bool> {
        self.tree.set_raw_meta(id, content)
    }

    fn remove(&mut self, id: Self::Id) -> io::Result<()> {
        self.tree.remove(id)
    }

    fn persist(&mut self) -> io::Result<()> {
        self.tree.persist()
    }

    fn persist_async(&mut self, callback: PersistCallbackFunc) {
        self.tree.persist_async(callback)
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

    fn update_meta(&mut self, id: Self::Id, prefix: &str, value: &str) -> io::Result<bool> {
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

#[cfg(test)]
mod tests {
    use crate::backend::tests::TestTreeBackend;
    const BACKEND_PY: &str = r#"
import collections

class NoteBackend:
    ROOT = 0

    def __init__(self):
        self.text = collections.defaultdict(str)
        self.meta = collections.defaultdict(str)
        self.mtime = collections.defaultdict(int)
        self.children = collections.defaultdict(list)
        self.next_id = 1
        self.parent = collections.defaultdict(int)

    def get_children(self, id):
        return self.children[id]

    def get_parent(self, id):
        if id == self.ROOT:
            return None
        return self.parent[id]

    def get_text(self, id):
        return self.text[id]

    def get_mtime(self, id):
        return self.mtime[id]

    def get_raw_meta(self, id):
        return self.meta[id]

    def insert(self, dest_id, pos, text, meta):
        id = self._next_free_id()
        self.text[id] = text
        self.meta[id] = meta
        if pos == 0:  # Append
            self.parent[id] = dest_id
            self.children[dest_id].append(id)
        else:
            parent_id = self.parent[dest_id]
            self.parent[id] = parent_id
            children = self.children[parent_id]
            idx = children.index(dest_id)
            if pos > 0:
                idx += 1
            children.insert(idx, id)
        self._bump_mtime(id)
        return id

    def set_parent(self, id, dest_id, pos):
        if self._is_ancestor(id, dest_id):
            raise "Cannot move to descent"
        self._bump_mtime(id)
        parent_id = self.parent[id]
        old_children = self.children[parent_id]
        if id in old_children:
            old_children.remove(id)
        if pos == 0:  # Append
            parent_id = dest_id
            self.parent[id] = parent_id
            self.children[parent_id].append(id)
        else:
            parent_id = self.parent[dest_id]
            self.parent[id] = parent_id
            children = self.children[parent_id]
            idx = children.index(dest_id)
            if pos > 0:  # After
                idx += 1
            children.insert(idx, id)
        self._bump_mtime(id)
        return id

    def set_text(self, id, text):
        if self.text[id] == text:
            return False
        self.text[id] = text
        self._bump_mtime(id)
        return True  # changed

    def set_raw_meta(self, id, meta):
        if self.meta[id] == meta:
            return False
        self.meta[id] = meta
        self._bump_mtime(id)
        return True  # changed

    def remove(self, id):
        if id == self.ROOT:
            return
        self._bump_mtime(id)
        parent_id = self.parent[id]
        old_children = self.children[parent_id]
        if id in old_children:
            old_children.remove(id)

    def persist(self, id):
        pass

    def _bump_mtime(self, id):
        while True:
            self.mtime[id] += 1
            pid = self.parent[id]
            if pid == id:
                break
            id = pid

    def _next_free_id(self):
        id = self.next_id
        self.next_id += 1
        return id

    def _is_ancestor(self, anc, id):
        while True:
            if id == anc:
                return True
            new_id = self.parent[id]
            if id == new_id:
                break
            id = new_id
        return False

def get_instance(url):
    return NoteBackend()
"#;
    #[test]
    fn test_basic() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path();
        let py_path = path.join("a.py");
        std::fs::write(&py_path, BACKEND_PY).unwrap();
        match crate::url::open(&py_path.display().to_string()) {
            Ok(mut backend) => {
                backend.check_generic().unwrap();
            }
            Err(e) => {
                eprintln!("skip: python backend is not available: {}", e);
            }
        }
    }
}
