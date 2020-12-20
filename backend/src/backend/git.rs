use super::Id;
use super::TreeBackend;
use git2::Oid;
use std::collections::HashMap;
use std::path::PathBuf;

/// Backend. Backed by a local or remote Git repo.
pub(crate) struct GitBackend {
    /// Remote URL
    url: String,

    /// Local path.
    path: PathBuf,
}

impl TreeBackend for GitBackend {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> std::io::Result<Vec<Self::Id>> {
        todo!()
    }

    fn get_parent(&self, id: Self::Id) -> std::io::Result<Option<Self::Id>> {
        todo!()
    }

    fn get_mtime(&self, id: Self::Id) -> std::io::Result<super::Mtime> {
        todo!()
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> std::io::Result<std::borrow::Cow<'a, str>> {
        todo!()
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> std::io::Result<std::borrow::Cow<'a, str>> {
        todo!()
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: super::InsertPos,
        text: String,
        meta: String,
    ) -> std::io::Result<Self::Id> {
        todo!()
    }

    fn set_parent(
        &mut self,
        id: Self::Id,
        dest_id: Self::Id,
        pos: super::InsertPos,
    ) -> std::io::Result<Self::Id> {
        todo!()
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> std::io::Result<()> {
        todo!()
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> std::io::Result<()> {
        todo!()
    }

    fn remove(&mut self, id: Self::Id) -> std::io::Result<()> {
        todo!()
    }

    fn persist(&mut self) -> std::io::Result<()> {
        todo!()
    }
}
