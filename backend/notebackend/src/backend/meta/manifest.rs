use crate::manifest::Manifest;
use crate::manifest::ROOT_ID;
use crate::manifest::TRASH_ID;
use crate::t;
use notebackend_types::Id;
use notebackend_types::InsertPos;
use notebackend_types::Mtime;
use notebackend_types::PersistCallbackFunc;
use notebackend_types::TreeBackend;
use std::io;

/// APIs about how to operate on "text".
pub trait TextIO: Send + Sync + 'static {
    fn get_raw_text<'a>(&'a self, id: Id) -> io::Result<std::borrow::Cow<'a, str>>;
    fn set_raw_text(&mut self, id: Id, text: String) -> io::Result<()>;
    fn remove_raw_text(&mut self, id: Id) -> io::Result<()>;
    fn persist_with_manifest(&mut self, manifest: &mut Manifest) -> io::Result<()>;
    fn persist_async_with_manifest(
        &mut self,
        manifest: &mut Manifest,
        callback: PersistCallbackFunc,
    ) {
        let r = self.persist_with_manifest(manifest);
        callback(r)
    }
    fn get_inline_data(&self) -> Option<&[u8]> {
        None
    }
}
/// Manifest-based tree backend. Includes how to deal with texts.
pub struct ManifestBasedBackend<T> {
    pub manifest: Manifest,
    pub text_io: T,
    read_only: bool,
    has_trash: bool,
}

impl<T> ManifestBasedBackend<T> {
    pub fn from_manifest_text_io(manifest: Manifest, text_io: T) -> Self {
        Self {
            manifest,
            text_io,
            read_only: false,
            has_trash: false,
        }
    }

    /// Mark as read-only.
    pub fn freeze(mut self) -> Self {
        self.read_only = true;
        self
    }

    /// Enable or disable trash.
    pub fn with_trash(mut self, enabled: bool) -> Self {
        self.has_trash = enabled;
        self
    }

    fn ensure_not_read_only(&self) -> io::Result<()> {
        if self.read_only {
            Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                t!(cn = "无法修改只读节点", en = "tree is read only"),
            ))
        } else {
            Ok(())
        }
    }
}

impl<T: TextIO> TreeBackend for ManifestBasedBackend<T> {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> io::Result<Vec<Self::Id>> {
        let mut children = self.manifest.get_children(id).to_vec();
        if id == ROOT_ID && self.has_trash && !self.manifest.get_children(TRASH_ID).is_empty() {
            children.push(TRASH_ID);
        }
        Ok(children)
    }

    fn get_parent(&self, id: Self::Id) -> io::Result<Option<Self::Id>> {
        let parent = if id == TRASH_ID {
            if self.has_trash {
                Some(ROOT_ID)
            } else {
                None
            }
        } else {
            self.manifest.get_parent(id)
        };
        Ok(parent)
    }

    fn get_mtime(&self, id: Self::Id) -> io::Result<Mtime> {
        Ok(self.manifest.mtime.get(&id).cloned().unwrap_or_default())
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        if id == TRASH_ID {
            return Ok(t!(cn = "回收站", en = "Trash",).into());
        }
        self.text_io.get_raw_text(id)
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        if id == TRASH_ID {
            return Ok("type=trash\ncopyable=false\npin=true\nreadonly=true\n".into());
        }

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
        self.ensure_not_read_only()?;
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
            InsertPos::Before => children
                .iter()
                .position(|&c| c == dest_id)
                .map(|i| i as isize)
                .unwrap_or(-1),
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
        self.ensure_not_read_only()?;
        if id == ROOT_ID || id == TRASH_ID {
            return notebackend_types::error::invalid_input(t!(
                cn = "无法移动特殊节点",
                en = "special nodes cannot be moved"
            ));
        }
        let parent_id = match pos {
            InsertPos::Before | InsertPos::After => self
                .get_parent(dest_id)?
                .unwrap_or_else(|| self.get_root_id()),
            InsertPos::Append => dest_id,
        };
        if self.is_ancestor(id, parent_id)? {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                t!(
                    cn = "无法将 {:?} ({}) 移动到其子节点 {:?} ({})",
                    en = "{:?} ({}) cannot be moved to be under its descendant {:?} ({})",
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

    fn set_text(&mut self, id: Self::Id, mut text: String) -> io::Result<bool> {
        // Normalize CRLF (from Windows) to LF.
        text = text.replace("\r\n", "\n");
        self.ensure_not_read_only()?;
        let orig_text = self.get_text(id)?;
        let mut changed = false;
        if orig_text.as_ref() != text.as_str() {
            self.text_io.set_raw_text(id, text)?;
            self.touch(id)?;
            changed = true;
        }
        Ok(changed)
    }

    fn set_raw_meta(&mut self, id: Self::Id, meta: String) -> io::Result<bool> {
        self.ensure_not_read_only()?;
        let orig_meta = self.get_raw_meta(id)?;
        let mut changed = false;
        if orig_meta != meta {
            self.manifest.metas.insert(id, meta);
            self.touch(id)?;
            changed = true;
        }
        Ok(changed)
    }

    fn remove(&mut self, id: Self::Id) -> io::Result<()> {
        self.ensure_not_read_only()?;
        if id == TRASH_ID {
            // Translate to "delete children of trash".
            for id in self.get_children(TRASH_ID)? {
                self.manifest.remove_parent(id);
                self.manifest.remove(id);
                self.text_io.remove_raw_text(id)?;
            }
            return Ok(());
        }
        if id == ROOT_ID {
            return notebackend_types::error::invalid_input(t!(
                cn = "无法删除特殊节点",
                en = "special nodes cannot be removed"
            ));
        }
        self.touch(id)?;
        let should_use_trash = if !self.has_trash {
            // Trash is disabled.
            false
        } else if self.is_ancestor(TRASH_ID, id)? {
            // Already in trash.
            false
        } else if self.get_children(id)?.is_empty() && self.get_text(id)?.is_empty() {
            // No children, no text - skip trash.
            false
        } else {
            true
        };
        if should_use_trash {
            // Move to trash.
            self.set_parent(id, TRASH_ID, InsertPos::Append)?;
        } else {
            // Already in trash, or trash disabled. Remove directly.
            self.manifest.remove_parent(id);
            self.manifest.remove(id);
            self.text_io.remove_raw_text(id)?;
        }
        Ok(())
    }

    fn persist(&mut self) -> io::Result<()> {
        let unreachable = self.manifest.remove_unreachable();
        for id in unreachable {
            self.text_io.remove_raw_text(id)?;
        }
        self.text_io.persist_with_manifest(&mut self.manifest)
    }

    fn persist_async(&mut self, callback: PersistCallbackFunc) {
        let unreachable = self.manifest.remove_unreachable();
        for id in unreachable {
            if let Err(e) = self.text_io.remove_raw_text(id) {
                callback(Err(e));
                return;
            }
        }
        self.text_io
            .persist_async_with_manifest(&mut self.manifest, callback);
    }

    fn touch(&mut self, id: Self::Id) -> io::Result<()> {
        self.ensure_not_read_only()?;
        self.manifest.touch(id);
        Ok(())
    }

    fn inline_data(&self) -> Option<&[u8]> {
        self.text_io.get_inline_data()
    }
}
