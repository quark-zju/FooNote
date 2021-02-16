//! Utilities to make clipboard operations (copy, paste) on backends easier.
//! For example, serialize selected nodes into a binary format.

use crate::backend::MemBackend;
use crate::t;
pub use notebackend_types::BackendId;
use notebackend_types::InsertPos;
use notebackend_types::TreeBackend;
use notebackend_types::TreeMeta;
use std::io;
use std::io::Result;

/// Copy copyable selected ids and descendants to a temporary backend.
pub fn copy<T>(src: &T, ids: &[T::Id]) -> Result<MemBackend>
where
    T: TreeBackend,
{
    log::debug!("copy {:?}", &ids);
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
        copy_replace(src, id, &mut dst, dst_id, None)?;
    }

    Ok(dst)
}

/// Paste content in a temporary backend to the given destination.
/// Return all ids added. Consider calling get_heads to simplify the ids.
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
        copy_replace(src, src_id, dst, new_dst_id, Some(&mut ids))?;
        pos = InsertPos::After;
        dst_id = new_dst_id;
    }
    Ok(ids)
}

/// Replace destination with the source, recursively.
/// The type signature ensures src and dst are different.
/// This is used by multiplex backend to implement moving.
pub(crate) fn copy_replace<S: TreeBackend, D: TreeBackend>(
    src: &S,
    src_id: S::Id,
    dst: &mut D,
    dst_id: D::Id,
    mut dst_new_ids: Option<&mut Vec<D::Id>>,
) -> Result<()> {
    if !src.is_copyable(src_id)? {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            t!(cn = "节点不可复制", en = "Node is not copyable",),
        ));
    }
    log::trace!("copy_replace {:?} => {:?}", &src_id, &dst_id);
    let text = src.get_text(src_id)?;
    let meta = src.get_raw_meta(src_id)?;
    dst.set_text(dst_id, text.to_string())?;
    dst.set_raw_meta(dst_id, meta.to_string())?;

    // Skip copying children if it's marked as not copyable.
    if !src.is_children_copyable(src_id)? {
        return Ok(());
    }

    // Ensure that they have the same number of children.
    let src_children: Vec<_> = src
        .get_children(src_id)?
        .into_iter()
        .filter(|&c| src.is_copyable(c).unwrap_or(true))
        .collect();
    let mut dst_children = dst.get_children(dst_id)?;
    while src_children.len() < dst_children.len() {
        if let Some(id) = dst_children.pop() {
            dst.remove(id)?;
        }
    }
    while src_children.len() > dst_children.len() {
        let id = dst.insert(
            dst_id,
            InsertPos::Append,
            Default::default(),
            Default::default(),
        )?;
        if let Some(ids) = dst_new_ids.as_deref_mut() {
            ids.push(id);
        }
        dst_children.push(id);
    }

    // Copy recursively.
    for (src_id, dst_id) in src_children.into_iter().zip(dst_children.into_iter()) {
        copy_replace(src, src_id, dst, dst_id, dst_new_ids.as_deref_mut())?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::tests::TestTreeBackend;
    use notebackend_types::Id;
    use notebackend_types::TreeBackend;

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
                \_ 1 ("b")
                \_ 2 ("c")
                |  \_ 3 ("e")
                |     \_ 4 ("f")
                \_ 5 ("g")"#
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
                |  |  \_ 8 ("b")
                |  |  \_ 9 ("c")
                |  |  |  \_ 10 ("e")
                |  |  |     \_ 11 ("f")
                |  |  \_ 12 ("g")
                |  |  \_ 5 ("e")
                |  |     \_ 6 ("f")
                |  \_ 4 ("d")
                |     \_ 7 ("g")
                \_ 2 ("b")"#
        );
    }
}
