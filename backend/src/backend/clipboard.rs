use super::blob::MemBackend;
use super::InsertPos;
use super::TreeBackend;
use std::io::Result;

/// Copy copyable selected ids and descendants to a temporary backend.
pub fn copy<T>(src: &T, ids: &[T::Id]) -> Result<MemBackend>
where
    T: TreeBackend,
{
    let head_ids = src.get_heads(ids)?;
    let mut dst = MemBackend::empty();
    for id in head_ids {
        let dst_id = dst.insert(
            dst.get_root_id(),
            InsertPos::Append,
            String::new(),
            String::new(),
        )?;
        // Heads are enough."copy" is recursive.
        super::copy(src, id, &mut dst, dst_id, None)?;
    }

    Ok(dst)
}

/// Paste content in a temporary backend to the given destination.
pub fn paste<T>(
    src: &MemBackend,
    dst: &mut T,
    mut dst_id: T::Id,
    mut pos: InsertPos,
) -> Result<Vec<T::Id>>
where
    T: TreeBackend,
{
    let mut ids = Vec::new();
    for src_id in src.get_children(src.get_root_id())? {
        let new_dst_id = dst.insert(dst_id, pos, String::new(), String::new())?;
        ids.push(new_dst_id);
        super::copy(src, src_id, dst, new_dst_id, Some(&mut ids))?;
        pos = InsertPos::After;
        dst_id = new_dst_id;
    }
    Ok(ids)
}

#[cfg(test)]
mod tests {
    use super::super::tests::TestTreeBackend;
    use super::super::Id;
    use super::super::TreeBackend;
    use super::*;

    fn quick_insert(b: &mut MemBackend) -> Vec<Id> {
        let id0 = b.get_root_id();
        let id1 = b.quick_insert(id0, "a");
        let id2 = b.quick_insert(id0, "b");
        let id3 = b.quick_insert(id1, "c");
        let id4 = b.quick_insert(id1, "d");
        let id5 = b.quick_insert(id3, "e");
        let id6 = b.quick_insert(id5, "f");
        let id7 = b.quick_insert(id4, "g");
        let ids = vec![id0, id1, id2, id3, id4, id5, id6, id7];
        return ids;
    }

    #[test]
    fn test_copy_paste() {
        let mut b = MemBackend::empty();

        let mut ids = quick_insert(&mut b);
        assert_eq!(
            b.draw_ascii(&ids),
            r#"
                root
                \_ 1 ("a")
                |  \_ 3 ("c")
                |  |  \_ 5 ("e")
                |  |     \_ 6 ("f")
                |  \_ 4 ("d")
                |     \_ 7 ("g")
                \_ 2 ("b")"#
        );

        let m = copy(&b, &[ids[2], ids[3], ids[6], ids[7]]).unwrap();
        assert_eq!(
            m.draw_ascii(&ids),
            r#"
                root
                \_ 1 ("c")
                |  \_ 2 ("e")
                |     \_ 3 ("f")
                \_ 4 ("g")
                \_ 5 ("b")"#
        );

        // Insert before "5" ("e")
        let inserted_ids = paste(&m, &mut b, ids[5], InsertPos::Before).unwrap();
        ids.extend(inserted_ids);
        assert_eq!(
            b.draw_ascii(&ids),
            r#"
                root
                \_ 1 ("a")
                |  \_ 3 ("c")
                |  |  \_ 8 ("c")
                |  |  |  \_ 9 ("e")
                |  |  |     \_ 10 ("f")
                |  |  \_ 11 ("g")
                |  |  \_ 12 ("b")
                |  |  \_ 5 ("e")
                |  |     \_ 6 ("f")
                |  \_ 4 ("d")
                |     \_ 7 ("g")
                \_ 2 ("b")"#
        );
    }
}
