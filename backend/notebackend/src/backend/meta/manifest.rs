use crate::manifest::Manifest;
use notebackend_types::Id;
use notebackend_types::InsertPos;
use notebackend_types::Mtime;
use notebackend_types::TreeBackend;
use std::io;

/// APIs about how to operate on "text".
pub trait TextIO: Send + Sync + 'static {
    fn get_raw_text<'a>(&'a self, id: Id) -> io::Result<std::borrow::Cow<'a, str>>;
    fn set_raw_text(&mut self, id: Id, text: String) -> io::Result<()>;
    fn remove_raw_text(&mut self, id: Id) -> io::Result<()>;
    fn persist_with_manifest(&mut self, manifest: &mut Manifest) -> io::Result<()>;
}
/// Manifest-based tree backend.
pub struct ManifestBasedBackend<T> {
    pub manifest: Manifest,
    pub text_io: T,
}

impl<T> ManifestBasedBackend<T> {
    pub fn from_manifest_text_io(manifest: Manifest, text_io: T) -> Self {
        Self { manifest, text_io }
    }
}

impl<T: TextIO> TreeBackend for ManifestBasedBackend<T> {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> io::Result<Vec<Self::Id>> {
        Ok(self.manifest.children.get(&id).cloned().unwrap_or_default())
    }

    fn get_parent(&self, id: Self::Id) -> io::Result<Option<Self::Id>> {
        Ok(self.manifest.parents.get(&id).cloned())
    }

    fn get_mtime(&self, id: Self::Id) -> io::Result<Mtime> {
        Ok(self.manifest.mtime.get(&id).cloned().unwrap_or_default())
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        self.text_io.get_raw_text(id)
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
            self.text_io.set_raw_text(id, text)?;
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
            self.text_io.set_raw_text(id, text)?;
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
        self.text_io.remove_raw_text(id)?;
        Ok(())
    }

    fn persist(&mut self) -> io::Result<()> {
        self.text_io.persist_with_manifest(&mut self.manifest)
    }

    fn touch(&mut self, id: Self::Id) -> io::Result<()> {
        self.manifest.touch(id);
        Ok(())
    }
}
