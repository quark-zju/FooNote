use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::io::Result;
use std::ops::Deref;
use std::ops::DerefMut;

pub type Id = i32;
pub type BackendId = usize;
pub type Mtime = i32;

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InsertPos {
    /// Before the given node.
    Before = 0,

    /// After the given node.
    After,

    /// Append to a "parent".
    Append,
}

impl From<InsertPos> for i32 {
    fn from(pos: InsertPos) -> Self {
        match pos {
            InsertPos::Before => -1,
            InsertPos::After => 1,
            InsertPos::Append => 0,
        }
    }
}

impl From<i32> for InsertPos {
    fn from(v: i32) -> Self {
        if v < 0 {
            Self::Before
        } else if v > 0 {
            Self::After
        } else {
            Self::Append
        }
    }
}

/// A tree backend maintains an ordered tree of nodes.
/// Each node has: text, metadata, and children.
/// The source of truth of the nodes are defined by the trait.
///
/// The tree backend itself does not detect special headers of text and
/// do things differently.
/// The frontend might do so, for example, use a different icon if the
/// text starts with "todo:".
pub trait TreeBackend: Send + Sync + 'static {
    type Id: Clone + Copy + Default + Eq + Hash + fmt::Debug + fmt::Debug + Send + Sync + 'static;

    /// The id of the root tree node. It should always match Id::default().
    fn get_root_id(&self) -> Self::Id {
        Self::Id::default()
    }

    /// Children of the selected node.
    fn get_children(&self, id: Self::Id) -> Result<Vec<Self::Id>>;

    /// Parent of the selected id.
    fn get_parent(&self, id: Self::Id) -> Result<Option<Self::Id>>;

    /// Test if an id is an ancestor of another id.
    fn is_ancestor(&self, ancestor: Self::Id, descendant: Self::Id) -> Result<bool> {
        if ancestor == descendant {
            Ok(true)
        } else {
            let mut id = descendant;
            loop {
                match self.get_parent(id)? {
                    None => break Ok(false),
                    Some(parent) => {
                        if parent == ancestor {
                            break Ok(true);
                        }
                        id = parent;
                    }
                }
            }
        }
    }

    /// Last modified time.
    fn get_mtime(&self, id: Self::Id) -> Result<Mtime>;

    /// Actual content.
    fn get_text<'a>(&'a self, id: Self::Id) -> Result<Cow<'a, str>>;

    /// Raw Metadata.
    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> Result<Cow<'a, str>>;

    /// Extract value from the metadata.
    fn extract_meta<'a>(&'a self, id: Self::Id, prefix: &str) -> Result<Cow<'a, str>> {
        let meta = self.get_raw_meta(id)?;
        match meta {
            Cow::Borrowed(meta) => Ok(Cow::Borrowed(extract_meta_text(meta, prefix))),
            Cow::Owned(meta) => Ok(extract_meta_text(&meta, prefix).to_string().into()),
        }
    }

    /// Update a meta value with the given key.
    fn update_meta(&mut self, id: Self::Id, prefix: &str, value: &str) -> Result<()> {
        if value.contains('\n') {
            return super::error::invalid_input("meta value cannot have multiple lines");
        }
        let meta = self.get_raw_meta(id)?;
        let mut new_meta = String::with_capacity(meta.len());
        let mut updated = false;
        for line in meta.lines() {
            if line.starts_with(prefix) {
                updated = true;
                if &line[prefix.len()..] == value {
                    // No need to update.
                    return Ok(());
                }
                if !value.is_empty() {
                    new_meta.push_str(prefix);
                    new_meta.push_str(value);
                    new_meta.push('\n');
                }
            } else {
                new_meta.push_str(line);
                new_meta.push('\n');
            }
        }
        if !updated && !value.is_empty() {
            new_meta.push_str(prefix);
            new_meta.push_str(value);
            new_meta.push('\n');
        }
        self.set_raw_meta(id, new_meta)
    }

    /// First (non-blank) line of the actual content. Usually used as title.
    fn get_text_first_line(&self, id: Self::Id) -> Result<String> {
        self.get_text(id).map(|s| {
            s.lines()
                .filter(|s| s.chars().any(|c| !c.is_whitespace()))
                .next()
                .map(|s| s.to_string())
                .unwrap_or_default()
        })
    }

    /// Create a new node.
    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: InsertPos,
        text: String,
        meta: String,
    ) -> Result<Self::Id>;

    /// Move `id` to the destination.
    fn set_parent(&mut self, id: Self::Id, dest_id: Self::Id, pos: InsertPos) -> Result<Self::Id>;

    /// Move `ids` to the `index`-th child of `parent_id`.
    fn set_parent_batch(
        &mut self,
        ids: &[Self::Id],
        mut dest_id: Self::Id,
        mut pos: InsertPos,
    ) -> Result<Vec<Self::Id>> {
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

    /// Update full text associated with `id`.
    fn set_text(&mut self, id: Self::Id, text: String) -> Result<()>;

    /// Set raw metadata.
    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> Result<()>;

    /// Update mtime of a node.
    fn touch(&mut self, id: Self::Id) -> Result<()> {
        let text = self.get_text(id)?.to_string();
        if text.is_empty() {
            self.set_text(id, "_".to_string())?;
        } else {
            self.set_text(id, "".to_string())?;
        }
        self.set_text(id, text)?;
        Ok(())
    }

    /// Remove an `id`.
    fn remove(&mut self, id: Self::Id) -> Result<()>;

    /// Remove in batch.
    fn remove_batch(&mut self, ids: &[Self::Id]) -> Result<()> {
        let ids = self.get_heads(ids)?;
        for id in ids.into_iter() {
            self.remove(id)?;
        }
        Ok(())
    }

    /// Auto fill an empty node with patterns from previous nodes.
    fn autofill(&mut self, id: Self::Id) -> Result<()> {
        if let Some(parent_id) = self.get_parent(id)? {
            if !self.get_text(id)?.is_empty() {
                return Ok(());
            }
            let children = self.get_children(parent_id)?;
            if let Some(pos) = children.iter().position(|&i| i == id) {
                let type_name = self.extract_meta(id, "type=")?;
                let titles = children
                    .iter()
                    .take(pos)
                    .rev()
                    .take_while(|&&c| match self.extract_meta(c, "type=") {
                        Err(_) => false,
                        Ok(s) => &type_name == &s,
                    })
                    .map(|&c| self.get_text_first_line(c))
                    .collect::<Result<Vec<_>>>()?;
                let seq: Vec<_> = titles.iter().rev().map(|s| s.as_str()).collect();
                if let Some(predict) = crate::util::autofill::autofill(&seq) {
                    self.set_text(id, predict)?;
                }
            }
        }
        Ok(())
    }

    /// Remove ids that are descendants of other ids in the given set.
    /// Return the sorted result.
    fn get_heads(&self, ids: &[Self::Id]) -> Result<Vec<Self::Id>> {
        let id_set: HashSet<Self::Id> = ids.iter().cloned().collect();
        let mut result = Vec::new();
        // Visit from the root (to preserve order).
        let mut to_visit = vec![self.get_root_id()];
        while let Some(id) = to_visit.pop() {
            if id_set.contains(&id) {
                result.push(id);
            } else {
                // Visit children in order.
                let mut children = self.get_children(id)?;
                children.reverse();
                to_visit.extend(children);
            }
        }
        Ok(result)
    }

    /// Write changes to the underlying backend.
    fn persist(&mut self) -> Result<()>;
}

impl TreeBackend for Box<dyn TreeBackend<Id = Id>> {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> Result<Vec<Self::Id>> {
        self.deref().get_children(id)
    }

    fn get_parent(&self, id: Self::Id) -> Result<Option<Self::Id>> {
        self.deref().get_parent(id)
    }

    fn get_mtime(&self, id: Self::Id) -> Result<Mtime> {
        self.deref().get_mtime(id)
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> Result<Cow<'a, str>> {
        self.deref().get_text(id)
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> Result<Cow<'a, str>> {
        self.deref().get_raw_meta(id)
    }

    fn get_heads(&self, ids: &[Self::Id]) -> Result<Vec<Self::Id>> {
        self.deref().get_heads(ids)
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: InsertPos,
        text: String,
        meta: String,
    ) -> Result<Self::Id> {
        self.deref_mut().insert(dest_id, pos, text, meta)
    }

    fn set_parent(&mut self, id: Self::Id, dest_id: Self::Id, pos: InsertPos) -> Result<Self::Id> {
        self.deref_mut().set_parent(id, dest_id, pos)
    }

    fn set_parent_batch(
        &mut self,
        ids: &[Self::Id],
        dest_id: Self::Id,
        pos: InsertPos,
    ) -> Result<Vec<Self::Id>> {
        self.deref_mut().set_parent_batch(ids, dest_id, pos)
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> Result<()> {
        self.deref_mut().set_text(id, text)
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> Result<()> {
        self.deref_mut().set_raw_meta(id, content)
    }

    fn remove(&mut self, id: Self::Id) -> Result<()> {
        self.deref_mut().remove(id)
    }

    fn persist(&mut self) -> Result<()> {
        self.deref_mut().persist()
    }
}

fn extract_meta_text<'a>(meta: &'a str, prefix: &str) -> &'a str {
    for line in meta.lines() {
        if line.starts_with(prefix) {
            return &line[prefix.len()..].trim();
        }
    }
    ""
}