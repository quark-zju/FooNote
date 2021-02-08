use notebackend_types::Id;
use notebackend_types::Mtime;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Manifest {
    #[serde(default)]
    pub children: BTreeMap<Id, Vec<Id>>,
    #[serde(default)]
    pub metas: BTreeMap<Id, String>,
    #[serde(default = "min_next_id")]
    pub next_id: Id,
    #[serde(skip)]
    pub parents: HashMap<Id, Id>, // derived from children
    #[serde(skip)]
    pub mtime: HashMap<Id, Mtime>, // temporary in-process state
    #[serde(default)]
    pub has_trash: bool,
}

pub const ROOT_ID: Id = 0;
pub const TRASH_ID: Id = 1;

impl PartialEq for Manifest {
    fn eq(&self, other: &Self) -> bool {
        // Do not test "parents" and "mtime" - they are derived.
        &self.children == &other.children
            && &self.metas == &other.metas
            && self.has_trash == other.has_trash
    }
}

impl Default for Manifest {
    fn default() -> Self {
        let mut result = Self {
            metas: Default::default(),
            children: Default::default(),
            parents: Default::default(),
            mtime: Default::default(),
            next_id: min_next_id(),
            has_trash: false,
        };
        result.metas.insert(ROOT_ID, "type=root\n".to_string());
        result.rebuild_parents();
        result
    }
}

impl Manifest {
    /// Enable or disable trash.
    pub fn with_trash(mut self, enabled: bool) -> Self {
        if self.has_trash != enabled {
            self.has_trash = enabled;
            if enabled {
                self.metas.insert(
                    TRASH_ID,
                    "type=trash\ncopyable=false\npin=true\nreadonly=true\n".to_string(),
                );
            }
        }
        self
    }

    pub fn get_children(&self, id: Id) -> Vec<Id> {
        let mut children = self.children.get(&id).cloned().unwrap_or_default();
        if id == ROOT_ID && self.has_trash {
            children.push(TRASH_ID);
        }
        children
    }

    pub fn get_parent(&self, id: Id) -> Option<Id> {
        if id == TRASH_ID {
            if self.has_trash {
                return Some(ROOT_ID);
            } else {
                return None;
            }
        }
        let parent_id = self.parents.get(&id).cloned().unwrap_or(id);
        if parent_id == id {
            None
        } else {
            Some(parent_id)
        }
    }

    /// Remove unreachable nodes. Return the unreachable Ids.
    pub fn remove_unreachable(&mut self) -> Vec<Id> {
        let unreachable = self.unreachable_ids(&[ROOT_ID, TRASH_ID]);
        for &id in &unreachable {
            self.remove(id);
        }
        unreachable
    }

    /// Rebuild parents from children data.
    pub fn rebuild_parents(&mut self) {
        for (&id, child_ids) in &self.children {
            for &child_id in child_ids {
                self.parents.insert(child_id, id);
            }
        }
    }

    /// Remove an id from its parent's children list.
    /// Useful for "move" or "remove" operations.
    pub fn remove_parent(&mut self, id: Id) {
        if let Some(parent_id) = self.parents.get(&id) {
            if let Some(children) = self.children.get_mut(parent_id) {
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

    /// Remove tracked metadata for `id`.
    pub fn remove(&mut self, id: Id) {
        self.parents.remove(&id);
        self.children.remove(&id);
        self.metas.remove(&id);
    }

    /// Clear the mtime state. Useful for tests.
    pub(crate) fn clear_mtime(&mut self) {
        self.mtime.clear();
    }

    /// Bump mtime of id and its ancestors.
    pub fn touch(&mut self, mut id: Id) {
        loop {
            self.mtime.entry(id).and_modify(|v| *v += 1).or_insert(1);
            if let Some(&parent_id) = self.parents.get(&id) {
                if parent_id != id {
                    id = parent_id;
                    continue;
                }
            }
            break;
        }
    }

    /// Find unreachable ids.
    pub fn unreachable_ids(&mut self, roots: &[Id]) -> Vec<Id> {
        let mut cache: BTreeMap<Id, bool> = BTreeMap::new();
        self.parents
            .keys()
            .cloned()
            .filter(|&id| !self.is_reachable_cached(id, &mut cache, roots))
            .collect()
    }

    fn is_reachable_cached(&self, id: Id, cache: &mut BTreeMap<Id, bool>, roots: &[Id]) -> bool {
        if roots.contains(&id) {
            return true;
        }
        match cache.get(&id) {
            Some(reachable) => *reachable,
            None => {
                let result = if let Some(&parent_id) = self.parents.get(&id) {
                    if parent_id == id {
                        false
                    } else {
                        self.is_reachable_cached(parent_id, cache, roots)
                    }
                } else {
                    false
                };
                cache.insert(id, result);
                result
            }
        }
    }

    /// Test if `id` is reachable from `ancestor_id`.
    pub fn is_reachable(&self, id: Id, ancestor_id: Id) -> bool {
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
}

/// Minimal `next_id`.
pub(crate) fn min_next_id() -> Id {
    10
}
