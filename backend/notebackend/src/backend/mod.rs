use crate::backend::meta::blob::BlobBackend;

mod dylib;
mod file;
mod git;
mod mem;
pub(crate) mod meta;
mod multiplex;
pub(crate) mod null;

pub use dylib::DylibBackend;
pub use git::GitBackend;
pub use multiplex::FullId;
pub use multiplex::MultiplexBackend;

pub type SingleFileBackend = BlobBackend<file::FileBlobIo>;
pub type MemBackend = BlobBackend<mem::MemBlobIo>;
pub type NamedMemBackend = BlobBackend<mem::NamedMemBlobIo>;

#[cfg(test)]
pub(crate) mod tests {
    use super::MemBackend;
    use crate::clipboard::copy_replace;
    use notebackend_types::Id;
    use notebackend_types::InsertPos;
    use notebackend_types::Mtime;
    use notebackend_types::TreeBackend;
    use std::collections::HashSet;
    use std::fmt;
    use std::io;

    impl PartialEq for MemBackend {
        fn eq(&self, other: &Self) -> bool {
            self.text_io.texts == other.text_io.texts && self.manifest == other.manifest
        }
    }

    impl fmt::Debug for MemBackend {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("MemBackend")?;
            self.text_io.blob_io.data.fmt(f)?;
            Ok(())
        }
    }

    // Abstract backend testing.
    pub(crate) trait TestTreeBackend: TreeBackend {
        /// Ensure children of `id` have `subset` but not other ids in `ids`.
        fn check_children(&self, id: Self::Id, subset: &[Self::Id], ids: &[Self::Id]) {
            let children = self.get_children(id).unwrap_or_default();
            for &cid in &children {
                assert_eq!(self.get_parent(cid).unwrap(), Some(id), "parent({:?})", cid);
            }
            let mut last_index = 0;
            for id in subset {
                let index = children.iter().position(|i| i == id).unwrap();
                assert!(index >= last_index);
                last_index = index;
            }
            for id in ids {
                if !subset.contains(id) {
                    assert!(!children.contains(id));
                }
            }
        }

        fn check_parent(&self, id: Self::Id, parent: Self::Id) {
            assert!(self.get_children(parent).unwrap().contains(&id));
            assert_eq!(self.get_parent(id).unwrap().unwrap(), parent);
        }

        /// Find id by title.
        fn find(&self, text: &str) -> Self::Id {
            if text == "root" {
                return self.get_root_id();
            }
            self.find_within(text, self.get_root_id())
                .expect(&format!("{} not found", text))
        }

        fn find_scoped(&self, text: &str, scope: &[Self::Id]) -> Self::Id {
            if text == "root" {
                return self.get_root_id();
            }
            self.find_within_scoped(text, self.get_root_id(), scope)
                .expect(&format!("{} not found", text))
        }

        /// Find ids by space-separated titles.
        fn find_ids(&self, texts: &str) -> Vec<Self::Id> {
            texts.split_whitespace().map(|t| self.find(t)).collect()
        }

        fn all_ids(&self) -> Vec<Self::Id> {
            let mut result = Vec::new();
            let mut visited = HashSet::new();
            let mut to_visit = vec![self.get_root_id()];
            while let Some(id) = to_visit.pop() {
                if visited.insert(id) {
                    result.push(id);
                    to_visit.extend(self.get_children(id).unwrap());
                }
            }
            result
        }

        fn find_ids_scoped(&self, texts: &str, scope: &[Self::Id]) -> Vec<Self::Id> {
            texts
                .split_whitespace()
                .map(|t| self.find_scoped(t, scope))
                .collect()
        }

        fn find_or_insert(
            &mut self,
            text: &str,
            parent: Self::Id,
            inserted: &mut Vec<Self::Id>,
        ) -> Self::Id {
            match self.find_within_scoped(text, self.get_root_id(), &inserted) {
                Some(id) => id,
                None => {
                    let id = self
                        .insert(parent, InsertPos::Append, text.to_string(), String::new())
                        .unwrap();
                    inserted.push(id);
                    id
                }
            }
        }

        fn find_within(&self, text: &str, id: Self::Id) -> Option<Self::Id> {
            if self.get_text(id).unwrap() == text {
                return Some(id);
            }
            let children = self.get_children(id).unwrap();
            for id in children {
                if let Some(id) = self.find_within(text, id) {
                    return Some(id);
                }
            }
            None
        }

        fn find_within_scoped(
            &self,
            text: &str,
            id: Self::Id,
            scope: &[Self::Id],
        ) -> Option<Self::Id> {
            if self.get_text(id).unwrap() == text {
                return Some(id);
            }
            let children = self.get_children(id).unwrap();
            for id in children.into_iter().filter(|i| scope.contains(i)) {
                if let Some(id) = self.find_within_scoped(text, id, scope) {
                    return Some(id);
                }
            }
            None
        }

        /// Return a subset of ids that have changed mtime.
        fn mtime_changed(
            &mut self,
            ids: &[Self::Id],
            mut func: impl FnMut(&mut Self),
        ) -> Vec<Self::Id> {
            let mtime_before: Vec<Mtime> =
                ids.iter().map(|id| self.get_mtime(*id).unwrap()).collect();
            func(self);
            let mtime_after: Vec<Mtime> =
                ids.iter().map(|id| self.get_mtime(*id).unwrap()).collect();
            ids.iter()
                .cloned()
                .enumerate()
                .filter_map(|(i, id)| {
                    if mtime_before[i] < mtime_after[i] {
                        Some(id)
                    } else {
                        None
                    }
                })
                .collect()
        }

        fn check_no_mtime_changed(&mut self, ids: &[Self::Id], func: impl Fn(&mut Self)) {
            assert!(self.mtime_changed(ids, func).is_empty());
        }

        fn check_mtime_changed(&mut self, id: Self::Id, func: impl Fn(&mut Self)) {
            assert!(self.mtime_changed(&[id], func).contains(&id));
        }

        /// `insert` with predefined text and meta.
        fn quick_insert(&mut self, parent_id: Self::Id, text: impl ToString) -> Self::Id {
            let text = text.to_string();
            let meta = format!("t={}", &text);
            let id = self
                .insert(parent_id, InsertPos::Append, text.clone(), meta.clone())
                .unwrap();
            assert_eq!(&text, &self.get_text(id).unwrap());
            assert_eq!(&meta, &self.get_raw_meta(id).unwrap());
            assert!(self.get_children(parent_id).unwrap().contains(&id));
            id
        }

        /// Check text and meta of id inserted by quick_insert.
        fn check_inserted_id(&mut self, id: Self::Id, text: impl ToString) {
            let text = text.to_string();
            let meta = format!("t={}", &text);
            assert_eq!(&text, &self.get_text(id).unwrap());
            assert_eq!(&meta, &self.get_raw_meta(id).unwrap());
        }

        fn check_update_id(&mut self, id: Self::Id, text: impl ToString) {
            let text = text.to_string();
            let meta = format!("t={}", &text);
            self.set_text(id, text.clone()).unwrap();
            self.set_raw_meta(id, meta.clone()).unwrap();
            assert_eq!(&text, &self.get_text(id).unwrap());
            assert_eq!(&meta, &self.get_raw_meta(id).unwrap());
        }

        fn check_extract_update_meta(&mut self, id: Self::Id) {
            let backup_existing_meta = self.get_raw_meta(id).unwrap().to_string();
            self.set_raw_meta(id, Default::default()).unwrap();
            assert_eq!(self.extract_meta(id, "foo=").unwrap(), "");
            self.check_mtime_changed(id, |s| assert!(s.update_meta(id, "foo=", "bar").unwrap()));
            self.check_mtime_changed(id, |s| assert!(s.update_meta(id, "foo2=", "bar2").unwrap()));
            self.check_no_mtime_changed(&[id], |s| {
                assert!(!s.update_meta(id, "foo3=", "").unwrap())
            });
            self.check_no_mtime_changed(&[id], |s| {
                assert!(!s.update_meta(id, "foo2=", "bar2").unwrap())
            });
            assert_eq!(self.extract_meta(id, "foo=").unwrap(), "bar");
            assert_eq!(self.extract_meta(id, "foo2=").unwrap(), "bar2");
            assert_eq!(self.extract_meta(id, "foo3=").unwrap(), "");
            assert_eq!(self.extract_meta(id, "foo4=").unwrap(), "");
            self.check_mtime_changed(id, |s| assert!(s.update_meta(id, "foo=", "baz").unwrap()));
            assert_eq!(self.extract_meta(id, "foo=").unwrap(), "baz");
            self.set_raw_meta(id, backup_existing_meta.to_string())
                .unwrap();
        }

        /// Check various basic APIs.
        fn check_generic(&mut self) -> io::Result<()> {
            let b = self;
            let root_id = b.get_root_id();
            // root
            // \_ 1
            // |  \_ 3 ("z")
            // |  \_ 4 ("a")
            // |     \_ 5
            // \_ 2 ("1")
            //    \_ 6
            let id1 = b.quick_insert(root_id, 1);
            let id2 = b.quick_insert(root_id, 1);
            let id3 = b.quick_insert(id1, "z");
            let id4 = b.quick_insert(id1, "a");
            let id5 = b.quick_insert(id4, 5);
            let id6 = b.quick_insert(id2, 6);
            let mut ids = vec![root_id, id1, id2, id3, id4, id5, id6];
            assert_eq!(
                b.draw_ascii(&ids),
                r#"
                root
                \_ 1
                |  \_ 3 ("z")
                |  \_ 4 ("a")
                |     \_ 5
                \_ 2 ("1")
                   \_ 6"#
            );

            // Read-only operations.
            b.check_no_mtime_changed(&ids, |b| {
                // Test children.
                b.check_children(root_id, &[id1, id2], &ids);
                b.check_children(id1, &[id3, id4], &ids);
                b.check_children(id4, &[id5], &ids);
                b.check_children(id2, &[id6], &ids);

                // Test reading content.
                b.check_inserted_id(id1, 1);
                b.check_inserted_id(id2, 1);
                b.check_inserted_id(id3, "z");
                b.check_inserted_id(id4, "a");
                b.check_inserted_id(id5, 5);
                b.check_inserted_id(id6, 6);
            });

            // Setting text, meta.
            let changed = b.mtime_changed(&ids, |b| {
                b.check_update_id(id4, "y");
            });
            assert_eq!(changed, vec![root_id, id1, id4]);
            b.check_extract_update_meta(id4);

            // Touch.
            let changed = b.mtime_changed(&ids, |b| {
                b.touch(id4).unwrap();
            });
            assert_eq!(changed, vec![root_id, id1, id4]);

            // Moving parents.
            // Swap 3 and 4.
            let changed = b.mtime_changed(&ids, |b| {
                b.set_parent(id4, id3, InsertPos::Before).unwrap();
            });
            assert_eq!(changed, vec![root_id, id1, id4]);
            b.check_children(id1, &[id4, id3], &ids);

            // Invalid move: 1 -> 5; root -> root; root -> 6; 2 -> 2;
            b.set_parent(id1, id5, InsertPos::Append).unwrap_err();
            b.set_parent(root_id, root_id, InsertPos::Append)
                .unwrap_err();
            b.set_parent(root_id, id6, InsertPos::Append).unwrap_err();
            b.set_parent(id2, id2, InsertPos::Append).unwrap_err();

            // Move 4 to 6.
            // root
            // \_ 1
            // |  \_ 3 ("z")
            // \_ 2 ("1")
            //    \_ 6
            //       \_ 4 ("y")
            //          \_ 5
            let changed = b.mtime_changed(&ids, |b| {
                b.set_parent(id4, id6, InsertPos::Append).unwrap();
            });
            assert_eq!(changed, vec![root_id, id1, id2, id4, id6]);
            b.check_children(id6, &[id4], &ids);
            b.check_children(id4, &[id5], &ids);
            b.check_children(id1, &[id3], &ids);
            assert_eq!(
                b.draw_ascii(&ids),
                r#"
                root
                \_ 1
                |  \_ 3 ("z")
                \_ 2 ("1")
                   \_ 6
                      \_ 4 ("y")
                         \_ 5"#
            );
            assert!(b.is_ancestor(root_id, id5).unwrap());
            assert!(b.is_ancestor(id2, id5).unwrap());
            assert!(!b.is_ancestor(id1, id5).unwrap());
            assert!(b.is_ancestor(id4, id4).unwrap());

            // Get and sort heads.
            assert_eq!(b.get_heads(&[id3, id2, id1]).unwrap(), vec![id1, id2]);
            assert_eq!(b.get_heads(&[id5, id6, id3]).unwrap(), vec![id3, id6]);

            // Deleting ids.
            let changed = b.mtime_changed(&ids, |b| {
                b.remove(id4).unwrap();
            });
            assert_eq!(changed, vec![root_id, id2, id4, id6]);
            b.check_children(id6, &[], &ids);
            assert_eq!(
                b.draw_ascii(&ids),
                r#"
                root
                \_ 1
                |  \_ 3 ("z")
                \_ 2 ("1")
                   \_ 6"#
            );

            // Inserting.
            // root
            // \_ 1
            // |  \_ 3 ("z")
            // \_ 2 ("1")
            //    \_ 6
            let mut id7 = Default::default();
            let changed = b.mtime_changed(&ids, |b| {
                id7 = b.quick_insert(id1, 7);
            });
            ids.push(id7);
            assert_eq!(changed, vec![root_id, id1]);
            b.check_children(id1, &[id3, id7], &ids);

            // Autofill.
            b.update_meta(id2, "type=", "sep").unwrap();
            let id8 = b.quick_insert(root_id, "10-15");
            let id9 = b.quick_insert(root_id, "10-16");
            let id10 = b.quick_insert(root_id, "");
            let id11 = b.quick_insert(root_id, "---");
            ids.extend_from_slice(&[id8, id9, id10, id11]);
            assert_eq!(b.get_text(id10).unwrap(), "");
            b.autofill(id10).unwrap();
            assert_eq!(b.get_text(id10).unwrap(), "10-17");
            assert_eq!(
                b.draw_ascii(&ids),
                r#"
                root
                \_ 1
                |  \_ 3 ("z")
                |  \_ 7
                \_ 2 ("1") (type=sep)
                |  \_ 6
                \_ 8 ("10-15")
                \_ 9 ("10-16")
                \_ 10 ("10-17")
                \_ 11 ("---")"#
            );

            // Remove in batch.
            b.remove_batch(&[id1, id2, id8, id9, id10, id11]).unwrap();
            assert_eq!(b.draw_ascii(&ids), "\n                root");

            // Set parents in batch (move).
            let mut ids = b.insert_ascii(
                r"
                                K
                               /
                A-B C-D-E F-G H-I L
                               \
                                J",
            );
            ids.insert(0, b.get_root_id());
            assert_eq!(
                b.draw_ascii(&ids),
                r#"
                root
                \_ 1 ("A")
                |  \_ 2 ("B")
                \_ 3 ("C")
                |  \_ 4 ("D")
                |     \_ 5 ("E")
                \_ 6 ("F")
                |  \_ 7 ("G")
                \_ 8 ("H")
                |  \_ 9 ("I")
                |  \_ 10 ("J")
                |  \_ 11 ("K")
                \_ 12 ("L")"#
            );
            b.set_parent_batch(
                &b.find_ids_scoped("E C A F G", &ids),
                b.find_scoped("I", &ids),
                InsertPos::After,
            )
            .unwrap();
            assert_eq!(
                b.draw_ascii(&ids),
                r#"
                root
                \_ 8 ("H")
                |  \_ 9 ("I")
                |  \_ 1 ("A")
                |  |  \_ 2 ("B")
                |  \_ 3 ("C")
                |  |  \_ 4 ("D")
                |  |     \_ 5 ("E")
                |  \_ 6 ("F")
                |  |  \_ 7 ("G")
                |  \_ 10 ("J")
                |  \_ 11 ("K")
                \_ 12 ("L")"#
            );

            Ok(())
        }

        fn insert_ascii(&mut self, ascii: &str) -> Vec<Self::Id> {
            let parents = drawdag::parse(ascii);
            let mut new_ids = Vec::new();
            for (child, parents) in parents {
                let id = self.find_or_insert(&child, self.get_root_id(), &mut new_ids);
                for p in parents {
                    let pid = self.find_or_insert(&p, self.get_root_id(), &mut new_ids);
                    if self.get_parent(id).unwrap().unwrap() != pid {
                        self.set_parent(id, pid, InsertPos::Append).unwrap();
                    }
                }
            }
            new_ids
        }

        fn draw_ascii(&self, ids: &[Self::Id]) -> String {
            let prefix = " ".repeat(16);
            let mut result = "\n".to_string();
            result.push_str(&prefix);
            self.draw_ascii_node(
                ids[0],
                ids,
                &prefix,
                &mut result,
                |this: &Self, id, ids: &[Self::Id]| {
                    let pos = ids.iter().position(|i| i == &id).unwrap();
                    if pos == 0 {
                        "root".to_string()
                    } else {
                        let text = this.get_text_first_line(id).unwrap();
                        let mut label = pos.to_string();
                        if text.parse::<usize>().ok() != Some(pos) {
                            label.push_str(&format!(" ({:?})", &text));
                        }
                        let type_name = this.extract_meta(id, "type=").unwrap();
                        if !type_name.is_empty() {
                            label.push_str(&format!(" (type={})", type_name));
                        }
                        label
                    }
                },
            );
            return result.trim_end().to_string();
        }

        fn draw_ascii_all(&self) -> String {
            let ids = self.all_ids();
            let prefix = " ".repeat(16);
            let mut result = "\n".to_string();
            result.push_str(&prefix);
            self.draw_ascii_node(
                ids[0],
                &ids,
                &prefix,
                &mut result,
                |this: &Self, id, ids: &[Self::Id]| {
                    let text = this.get_text(id).unwrap();
                    let meta = this.get_raw_meta(id).unwrap();
                    let meta = if meta.is_empty() {
                        "".to_string()
                    } else {
                        format!(" meta={:?}", meta)
                    };
                    format!("{:?} text={:?}{}", id, text, meta)
                },
            );
            return result.trim_end().to_string();
        }

        fn draw_ascii_node(
            &self,
            id: Self::Id,
            ids: &[Self::Id],
            prefix: &str,
            out: &mut String,
            label_func: fn(&Self, Self::Id, &[Self::Id]) -> String,
        ) {
            let pos = ids.iter().position(|i| i == &id).unwrap();
            let label = label_func(self, id, ids);
            out.push_str(&label);
            out.push('\n');
            let children: Vec<_> = self
                .get_children(id)
                .unwrap()
                .into_iter()
                .filter(|i| ids.contains(i))
                .collect();
            for (i, child) in children.iter().enumerate() {
                out.push_str(prefix);
                out.push_str("\\_ ");
                let new_prefix = if i + 1 == children.len() {
                    format!("{}   ", prefix)
                } else {
                    format!("{}|  ", prefix)
                };
                self.draw_ascii_node(*child, ids, &new_prefix, out, label_func);
            }
        }
    }

    impl<T: TreeBackend> TestTreeBackend for T {}

    #[test]
    fn test_copy_tree() {
        let mut a = MemBackend::empty();
        let mut b = MemBackend::empty();

        fn insert_ids(backend: &mut MemBackend, id0: Id) -> Vec<Id> {
            let id1 = backend.quick_insert(id0, "x");
            let id2 = backend.quick_insert(id0, "y");
            let id3 = backend.quick_insert(id1, "z");
            vec![id0, id1, id2, id3]
        }

        // Prepare
        let a0 = a.get_root_id();
        let al = insert_ids(&mut a, a0);
        assert_eq!(
            a.draw_ascii(&al),
            r#"
                root
                \_ 1 ("x")
                |  \_ 3 ("z")
                \_ 2 ("y")"#
        );

        let b0 = b.get_root_id();
        let mut bl = insert_ids(&mut b, b0);

        // Copy. "Trash" is not copied.
        let bd = bl[1];
        copy_replace(&a, a0, &mut b, bd, Some(&mut bl)).unwrap();
        assert_eq!(
            b.draw_ascii(&bl),
            r#"
                root
                \_ 1 ("") (type=root)
                |  \_ 3 ("x")
                |  |  \_ 5 ("z")
                |  \_ 4 ("y")
                \_ 2 ("y")"#
        );
    }

    #[test]
    fn test_insert_ascii() {
        let mut b = MemBackend::empty();
        let mut ids = vec![b.get_root_id()];
        ids.extend(b.insert_ascii(
            r#"
            A---B---C---G
                 \
                  D---E
                   \
                    F"#,
        ));
        assert_eq!(
            b.draw_ascii(&ids),
            r#"
                root
                \_ 1 ("A")
                   \_ 2 ("B")
                      \_ 3 ("C")
                      |  \_ 7 ("G")
                      \_ 4 ("D")
                         \_ 5 ("E")
                         \_ 6 ("F")"#
        );

        ids.extend(b.insert_ascii(r#"A---X  B---Y"#));
        assert_eq!(
            b.draw_ascii(&ids),
            r#"
                root
                \_ 1 ("A")
                |  \_ 2 ("B")
                |     \_ 3 ("C")
                |     |  \_ 7 ("G")
                |     \_ 4 ("D")
                |        \_ 5 ("E")
                |        \_ 6 ("F")
                \_ 8 ("A")
                |  \_ 10 ("X")
                \_ 9 ("B")
                   \_ 11 ("Y")"#
        );
    }
}
