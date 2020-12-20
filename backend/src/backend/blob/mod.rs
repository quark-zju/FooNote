mod file;
mod mem;

use super::*;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::io::{self, Read, Result, Write};

pub type SingleFileBackend = BlobBackend<file::FileBlobIo>;
pub type MemBackend = BlobBackend<mem::MemBlobIo>;

/// Backend that loads and dumps from a single blob.
pub struct BlobBackend<I> {
    blob_io: I,
    data: TreeData,
    mtime: BTreeMap<Id, Mtime>,
    has_trash: bool,
}

/// How to read or write a blob
pub trait BlobIo: Send + Sync + 'static {
    /// Load blob from backend.
    fn load(&mut self) -> io::Result<Box<dyn AsRef<[u8]>>>;

    /// Write data to backend.
    fn save(&mut self, data: Vec<u8>) -> io::Result<()>;
}

/// Tree Data that can be converted from/to bytes.
#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct TreeData {
    next_id: Id,
    texts: BTreeMap<Id, String>,
    metas: BTreeMap<Id, String>,
    #[serde(skip)]
    parents: BTreeMap<Id, Id>,
    children: BTreeMap<Id, Vec<Id>>,
}

const ROOT_ID: Id = 0;
const TRASH_ID: Id = 1;

impl Default for TreeData {
    fn default() -> Self {
        let mut result = Self {
            texts: Default::default(),
            metas: Default::default(),
            children: Default::default(),
            parents: Default::default(),
            next_id: TRASH_ID + 1,
        };
        result.metas.insert(ROOT_ID, "type=root\n".to_string());
        result.rebuild_parents();
        result
    }
}

const HEADER_V1: &[u8] = b"FOONOTE1\n\0";

impl TreeData {
    /// Load from a reader.
    pub fn load(reader: &mut impl Read) -> Result<Self> {
        let mut header = vec![0u8; HEADER_V1.len()];
        reader.read_exact(&mut header)?;
        if header != HEADER_V1 {
            return crate::error::invalid_data("invalid header");
        }
        let mut d = varbincode::de::Deserializer::new(reader);
        let mut data: TreeData = serde::Deserialize::deserialize(&mut d)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        data.rebuild_parents();
        Ok(data)
    }

    /// Dump the content to a file.
    pub fn dump(&self, writer: &mut impl Write) -> Result<()> {
        writer.write_all(HEADER_V1)?;
        let mut s = varbincode::ser::Serializer::new(writer);
        self.serialize(&mut s)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }

    /// Rebuild children from parents data.
    fn rebuild_parents(&mut self) {
        let mut parents: BTreeMap<Id, Id> = BTreeMap::new();
        for (parent_id, children) in &self.children {
            for id in children {
                parents.insert(*id, *parent_id);
            }
        }
        self.parents = parents;
    }

    /// Find unreachable ids.
    fn unreachable_ids(&mut self) -> Vec<Id> {
        let mut cache: BTreeMap<Id, bool> = BTreeMap::new();
        self.parents
            .keys()
            .cloned()
            .filter(|&id| !self.is_reachable_cached(id, &mut cache))
            .collect()
    }

    fn is_reachable_cached(&self, id: Id, cache: &mut BTreeMap<Id, bool>) -> bool {
        if id == ROOT_ID || id == TRASH_ID {
            return true;
        }
        match cache.get(&id) {
            Some(reachable) => *reachable,
            None => {
                let result = if let Some(&parent_id) = self.parents.get(&id) {
                    if parent_id == id {
                        false
                    } else {
                        self.is_reachable_cached(parent_id, cache)
                    }
                } else {
                    false
                };
                cache.insert(id, result);
                result
            }
        }
    }

    fn is_reachable(&self, id: Id, ancestor_id: Id) -> bool {
        if id == ancestor_id {
            return true;
        }
        let parent_id = self.parents.get(&id).cloned().unwrap_or(id);
        if parent_id == ancestor_id {
            true
        } else if parent_id == id {
            false
        } else {
            self.is_reachable(parent_id, ancestor_id)
        }
    }

    /// Remove parent of an id.
    /// Move to trash on the first time. When deleting again, make
    /// it "unreachable".
    fn remove_parent(&mut self, id: Id) {
        if let Some(parent_id) = self.parents.get(&id) {
            if let Some(children) = self.children.get_mut(parent_id) {
                // not stable yet: children.remove_item(&id);
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
        self.texts.remove(&id);
        self.metas.remove(&id);
    }
}

impl<I> BlobBackend<I> {
    /// Check if an Id is valid.
    fn check_id(&self, id: Id) -> Result<()> {
        if id >= self.data.next_id {
            Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("id {} does not exist", id),
            ))
        } else {
            Ok(())
        }
    }

    /// Remove unreachable nodes.
    pub fn remove_unreachable(&mut self) {
        let unreachable = self.data.unreachable_ids();
        for id in unreachable {
            self.mtime.remove(&id);
            self.data.remove(id);
        }
    }

    /// Converts to bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        self.data.dump(&mut buf).unwrap();
        buf
    }
}

impl<I> BlobBackend<I>
where
    I: BlobIo,
{
    pub fn from_blob_io(mut blob_io: I) -> Result<Self> {
        let bytes = blob_io.load()?;
        let mut buf = (*bytes).as_ref();
        let data: TreeData = if buf.is_empty() {
            Default::default()
        } else {
            TreeData::load(&mut buf)?
        };
        let result = Self {
            data,
            blob_io,
            mtime: Default::default(),
            has_trash: false,
        };
        Ok(result)
    }

    /// Enable or disable trash.
    pub fn with_trash(mut self, enabled: bool) -> Self {
        self.has_trash = enabled;
        if enabled {
            self.data.metas.insert(
                TRASH_ID,
                "type=trash\ncopyable=false\npin=true\n".to_string(),
            );
        }
        self
    }
}

impl<I> TreeBackend for BlobBackend<I>
where
    I: BlobIo,
{
    type Id = Id;

    fn get_root_id(&self) -> Id {
        ROOT_ID
    }

    fn get_children(&self, id: Id) -> Result<Vec<Id>> {
        let mut children = self.data.children.get(&id).cloned().unwrap_or_default();
        if id == ROOT_ID && self.has_trash {
            children.push(TRASH_ID);
        }
        Ok(children)
    }

    fn get_parent(&self, id: Id) -> Result<Option<Id>> {
        if id == TRASH_ID {
            if self.has_trash {
                return Ok(Some(ROOT_ID));
            } else {
                return Ok(None);
            }
        }
        let parent_id = self.data.parents.get(&id).cloned().unwrap_or(id);
        if parent_id == id {
            Ok(None)
        } else {
            Ok(Some(parent_id))
        }
    }

    fn get_mtime(&self, id: Id) -> Result<Mtime> {
        Ok(self.mtime.get(&id).cloned().unwrap_or_default())
    }

    fn get_text(&self, id: Id) -> Result<Cow<str>> {
        let s = self.data.texts.get(&id).map(|s| s.as_str()).unwrap_or("");
        Ok(Cow::Borrowed(s))
    }

    fn get_raw_meta(&self, id: Id) -> Result<Cow<str>> {
        let s = self.data.metas.get(&id).map(|s| s.as_str()).unwrap_or("");
        Ok(Cow::Borrowed(s))
    }

    fn insert(&mut self, dest_id: Id, pos: InsertPos, text: String, meta: String) -> Result<Id> {
        self.check_id(dest_id)?;
        let id = self.data.next_id;
        self.data.next_id += 1;

        let parent_id = match pos {
            InsertPos::Append => dest_id,
            InsertPos::Before | InsertPos::After => self
                .get_parent(dest_id)?
                .unwrap_or_else(|| self.get_root_id()),
        };
        let children = self.data.children.entry(parent_id).or_default();
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
        self.data.parents.insert(id, parent_id);
        if !text.is_empty() {
            self.set_text(id, text)?;
        }
        if !meta.is_empty() {
            self.set_raw_meta(id, meta)?;
        }
        self.touch(id)?;
        Ok(id)
    }

    fn set_text(&mut self, id: Id, text: String) -> Result<()> {
        self.check_id(id)?;
        let orig_text = self.get_text(id)?;
        if orig_text != text {
            self.data.texts.insert(id, text);
            self.touch(id)?;
        }
        Ok(())
    }

    fn set_raw_meta(&mut self, id: Id, meta: String) -> Result<()> {
        self.check_id(id)?;
        let orig_meta = self.get_raw_meta(id)?;
        if orig_meta != meta {
            self.data.metas.insert(id, meta);
            self.touch(id)?;
        }
        Ok(())
    }

    fn touch(&mut self, mut id: Self::Id) -> Result<()> {
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

    fn remove(&mut self, id: Id) -> Result<()> {
        self.check_id(id)?;
        if id == ROOT_ID || id == TRASH_ID {
            return crate::error::invalid_input("special nodes cannot be removed");
        }
        self.touch(id)?;
        if !self.has_trash || self.is_ancestor(TRASH_ID, id)? {
            // Already in trash, or trash disabled. Remove directly.
            self.data.remove_parent(id);
        } else {
            // Move to trash.
            self.set_parent(id, TRASH_ID, InsertPos::Append)?;
        }
        Ok(())
    }

    fn set_parent(&mut self, id: Id, dest_id: Id, pos: InsertPos) -> Result<Id> {
        self.check_id(id)?;
        self.check_id(dest_id)?;
        if id == ROOT_ID || id == TRASH_ID {
            return crate::error::invalid_input("special nodes cannot be moved");
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
        self.data.remove_parent(id);
        self.data.parents.insert(id, parent_id);
        let children = self.data.children.entry(parent_id).or_default();
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

    fn persist(&mut self) -> Result<()> {
        self.remove_unreachable();
        let mut buf: Vec<u8> = Default::default();
        self.data.dump(&mut buf)?;
        self.blob_io.save(buf)?;
        Ok(())
    }
}
