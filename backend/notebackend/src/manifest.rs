use notebackend_types::Id;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Default)]
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
}

/// Minimal `next_id`.
pub(crate) fn min_next_id() -> Id {
    10
}
