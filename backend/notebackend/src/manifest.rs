use notebackend_types::Id;
use notebackend_types::Mtime;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;

/// Metadata about the tree. Does not include the actual "text" of nodes.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Manifest {
    #[serde(default)]
    pub children: BTreeMap<Id, Vec<Id>>,
    #[serde(default)]
    pub metas: BTreeMap<Id, String>,
    #[serde(default = "min_next_id")]
    pub next_id: Id,
    #[serde(skip)]
    pub parents: BTreeMap<Id, Id>, // derived from children
    #[serde(skip)]
    pub mtime: HashMap<Id, Mtime>, // temporary in-process state
}

pub const ROOT_ID: Id = 0;
pub const TRASH_ID: Id = 1;

impl PartialEq for Manifest {
    fn eq(&self, other: &Self) -> bool {
        // Do not test "parents" and "mtime" - they are derived.
        &self.children == &other.children && &self.metas == &other.metas
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
        };
        result.metas.insert(ROOT_ID, "type=root\n".to_string());
        result.rebuild_parents();
        result
    }
}

impl Manifest {
    pub fn get_children(&self, id: Id) -> &[Id] {
        static EMPTY: Vec<Id> = Vec::new();
        self.children.get(&id).unwrap_or(&EMPTY)
    }

    pub fn get_parent(&self, id: Id) -> Option<Id> {
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

    /// Fix up broken data:
    /// - Children DAG should be a tree, not a graph.
    /// - next_id should not overlap with existing ids.
    pub(crate) fn ensure_tree_and_next_id(&mut self) {
        struct State<'a> {
            this: &'a mut Manifest,
            visited: HashSet<Id>,
            next_id: Id,
        }
        impl<'a> State<'a> {
            fn visit(&mut self, id: Id) {
                if id + 1 > self.next_id {
                    self.next_id = id + 1;
                }
                self.visited.insert(id);
                if let Some(child_ids) = self.this.children.get(&id) {
                    let mut new_child_ids = Vec::with_capacity(child_ids.len());
                    for &child_id in child_ids {
                        if self.visited.insert(child_id) {
                            new_child_ids.push(child_id);
                        }
                    }
                    for &child_id in &new_child_ids {
                        self.visit(child_id)
                    }
                    if new_child_ids.is_empty() {
                        self.this.children.remove(&id);
                    } else {
                        self.this.children.insert(id, new_child_ids);
                    }
                }
            }
        }
        let mut state = State {
            this: self,
            visited: HashSet::new(),
            next_id: min_next_id(),
        };
        state.visit(ROOT_ID);
        state.visit(TRASH_ID);
        self.next_id = state.next_id;
    }

    /// Rebuild parents from children data.
    /// Also fix other issues. See fix_invalid_data.
    pub fn rebuild_parents(&mut self) {
        self.ensure_tree_and_next_id();
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
            if id == TRASH_ID {
                id = ROOT_ID;
                continue;
            } else if let Some(parent_id) = self.get_parent(id) {
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

    /// Merge two manifests. Best effort. Do not cause conflicts.
    pub fn merge(&mut self, other: &Self) {
        // Merge "children".
        for (&id, theirs) in other.children.iter() {
            self.children
                .entry(id)
                .and_modify(|ours| {
                    if ours != theirs {
                        *ours = naive_merge(ours, theirs);
                    }
                })
                .or_insert_with(|| theirs.clone());
        }
        // Merge "metas".
        for (&id, theirs) in other.metas.iter() {
            self.metas
                .entry(id)
                .and_modify(|ours| {
                    if ours != theirs {
                        let theirs_lines: Vec<&str> = theirs.lines().collect();
                        let ours_lines: Vec<&str> = ours.lines().collect();
                        let merged_lines = naive_merge(&ours_lines, &theirs_lines);
                        let merged = merged_lines
                            .into_iter()
                            .map(|l| format!("{}\n", l))
                            .collect::<Vec<_>>()
                            .concat();
                        *ours = merged;
                    }
                })
                .or_insert_with(|| theirs.clone());
        }
        self.next_id = self.next_id.max(other.next_id);
        self.rebuild_parents();
    }

    /// Replace Ids using the mapping.
    pub fn remap_ids(&mut self, map: &HashMap<Id, Id>) {
        let remap = |id: Id| -> Id { map.get(&id).cloned().unwrap_or(id) };

        for &id in map.values() {
            if id + 1 > self.next_id {
                self.next_id = id + 1;
            }
        }

        for (_, children) in self.children.iter_mut() {
            for c in children {
                *c = remap(*c)
            }
        }

        self.children = self
            .children
            .iter()
            .map(|(k, v)| (remap(*k), v.clone()))
            .collect();

        self.metas = self
            .metas
            .iter()
            .map(|(k, v)| (remap(*k), v.clone()))
            .collect();

        self.mtime = self
            .mtime
            .iter()
            .map(|(k, v)| (remap(*k), v.clone()))
            .collect();

        self.parents = self
            .parents
            .iter()
            .map(|(k, v)| (remap(*k), remap(*v)))
            .collect();

        self.rebuild_parents();
    }
}

/// Naive merge algorithm that just keep all entries.
/// Attempt to preserve orders. Assumes that one item only appears once (!).
fn naive_merge<T>(a1: &[T], a2: &[T]) -> Vec<T>
where
    T: Clone + Eq + Hash,
{
    // &T -> index.
    let m2: HashMap<&T, usize> = a2.iter().enumerate().map(|(i, v)| (v, i)).collect();

    let mut result = Vec::with_capacity(a1.len().max(a2.len()));
    let mut dedup = HashSet::with_capacity(result.len());
    let mut i1 = 0;
    let mut i2 = 0;

    while i1 < a1.len() || i2 < a2.len() {
        if i1 == a1.len() {
            let v2 = &a2[i2];
            if dedup.insert(v2) {
                result.push(v2.clone());
            }
            i2 += 1;
            continue;
        }
        if i2 == a2.len() {
            let v1 = &a1[i1];
            if dedup.insert(v1) {
                result.push(v1.clone());
            }
            i1 += 1;
            continue;
        }
        let v1 = &a1[i1];
        let v2 = &a2[i2];
        if v1 == v2 {
            result.push(v1.clone());
            i1 += 1;
            i2 += 1;
            continue;
        }
        let mut pick = 1;
        if let Some(&i) = m2.get(v1) {
            if i > i2 {
                // v1 appears later in a2.
                pick = 2;
            }
        }
        if pick == 1 {
            if dedup.insert(v1) {
                result.push(v1.clone());
            }
            i1 += 1;
        } else {
            if dedup.insert(v2) {
                result.push(v2.clone());
            }
            i2 += 1;
        }
    }

    result
}

/// Minimal `next_id`.
pub(crate) fn min_next_id() -> Id {
    10
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ensure_tree() {
        let mut manifest = Manifest::default();
        manifest.children.insert(10, vec![10, 10, 10]);
        manifest.children.insert(20, vec![30]);
        manifest.children.insert(30, vec![40]);
        manifest.children.insert(40, vec![20, 50]);
        manifest.children.insert(ROOT_ID, vec![10, 30, 40, 20]);
        manifest.rebuild_parents();
        assert_eq!(manifest.next_id, 51);
        assert_eq!(
            format!("{:?}", &manifest.children),
            "{0: [10, 30, 40, 20], 40: [50]}"
        );
        assert_eq!(
            format!("{:?}", &manifest.parents),
            "{10: 0, 20: 0, 30: 0, 40: 0, 50: 40}"
        );
    }

    #[test]
    fn test_naive_merge() {
        let a = vec![1, 3, 2, 5];
        let b = vec![1, 3, 4, 2, 6, 5, 7];
        assert_eq!(naive_merge(&a, &b), [1, 3, 4, 2, 6, 5, 7]);
        assert_eq!(naive_merge(&b, &a), [1, 3, 4, 2, 6, 5, 7]);

        let a = vec![1, 2, 3, 5];
        let b = vec![1, 3, 4, 5];
        assert_eq!(naive_merge(&a, &b), [1, 2, 3, 4, 5]);
        assert_eq!(naive_merge(&b, &a), [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_manifest_merge() {
        let mut m1 = Manifest::default();
        m1.children.insert(10, vec![20, 30, 50, 60]);
        m1.children.insert(20, vec![21]);
        m1.children.insert(ROOT_ID, vec![10, 20, 30, 50, 60]);
        m1.metas.insert(10, "foo=1\nbar=2\n".into());
        m1.metas.insert(20, "foo=1\nbar=2\n".into());
        m1.next_id = 50;
        m1.rebuild_parents();

        let mut m2 = Manifest::default();
        m2.children.insert(10, vec![30, 40, 50]);
        m2.children.insert(21, vec![20, 23]);
        m2.children.insert(ROOT_ID, vec![10, 21, 20, 30, 40, 50]);
        m2.metas.insert(10, "baz=3\nbar=2\nzoo=4\n".into());
        m2.metas.insert(30, "foo=1\nbar=2\n".into());
        m2.next_id = 60;
        m1.merge(&m2);

        assert_eq!(
            format!("{:?}", &m1.children),
            "{0: [10, 21, 20, 30, 40, 50, 60], 21: [23]}"
        );
        assert_eq!(
            format!("{:?}", &m1.parents),
            "{10: 0, 20: 0, 21: 0, 23: 21, 30: 0, 40: 0, 50: 0, 60: 0}"
        );
        assert_eq!(
            format!("{:?}", &m1.metas),
            r#"{0: "type=root\n", 10: "foo=1\nbaz=3\nbar=2\nzoo=4\n", 20: "foo=1\nbar=2\n", 30: "foo=1\nbar=2\n"}"#
        );
        assert_eq!(m1.next_id, 61);
    }

    #[test]
    fn test_manifest_remap() {
        let mut m1 = Manifest::default();
        m1.children.insert(10, vec![20]);
        m1.children.insert(20, vec![30]);
        m1.children.insert(30, vec![40]);
        m1.children.insert(ROOT_ID, vec![10]);
        m1.metas.insert(10, "foo=1\n".into());
        m1.metas.insert(20, "foo=2\n".into());
        m1.metas.insert(30, "foo=3\n".into());
        let map: HashMap<Id, Id> = vec![(10, 11), (30, 33)].into_iter().collect();
        m1.remap_ids(&map);
        assert_eq!(
            format!("{:?}", &m1.children),
            "{0: [11], 11: [20], 20: [33], 33: [40]}"
        );
        assert_eq!(
            format!("{:?}", &m1.parents),
            "{11: 0, 20: 11, 33: 20, 40: 33}"
        );
        assert_eq!(
            format!("{:?}", &m1.metas),
            "{0: \"type=root\\n\", 11: \"foo=1\\n\", 20: \"foo=2\\n\", 33: \"foo=3\\n\"}"
        );
        assert_eq!(m1.next_id, 41);
    }
}
