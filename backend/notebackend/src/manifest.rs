use notebackend_types::Id;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Manifest {
    #[serde(default)]
    pub children: BTreeMap<Id, Vec<Id>>,
    #[serde(default)]
    pub metas: BTreeMap<Id, String>,
    #[serde(default = "min_next_id")]
    pub next_id: Id,
    #[serde(skip)]
    pub parents: HashMap<Id, Id>, // derived from children
}

const ROOT_ID: Id = 0;
const TRASH_ID: Id = 1;

impl Default for Manifest {
    fn default() -> Self {
        let mut result = Self {
            metas: Default::default(),
            children: Default::default(),
            parents: Default::default(),
            next_id: min_next_id(),
        };
        result.metas.insert(ROOT_ID, "type=root\n".to_string());
        result.rebuild_parents();
        result
    }
}

impl Manifest {
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
