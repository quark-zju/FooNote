//! Conflict-free merges that simply keep changes from both sides.

use std::char;
use std::collections::HashMap;
use std::hash::Hash;

/// Merge characters in strings. Keep both.
pub fn merge_two_strings(a: &str, b: &str) -> String {
    dissimilar::diff(a, b)
        .into_iter()
        .map(|c| match c {
            dissimilar::Chunk::Equal(s)
            | dissimilar::Chunk::Delete(s)
            | dissimilar::Chunk::Insert(s) => s,
        })
        .collect::<Vec<&str>>()
        .concat()
}

/// Calculate the difference between two lists.
pub fn diff_list<T>(a: &[T], b: &[T]) -> Vec<ChunkT<Vec<T>>>
where
    T: Clone + Eq + Hash,
{
    // Translate a, b to strings.
    let mut m = HashMap::<&T, u32>::with_capacity(a.len());

    for (i, t) in a.iter().enumerate() {
        m.entry(t).or_insert((i * 2) as u32);
    }
    for (i, t) in b.iter().enumerate() {
        m.entry(t).or_insert((i * 2 + 1) as u32);
    }

    const START: u32 = 0x10000;
    let sa: String = a
        .iter()
        .map(|t| char::from_u32(m[t] + START).unwrap())
        .collect();
    let sb: String = b
        .iter()
        .map(|t| char::from_u32(m[t] + START).unwrap())
        .collect();
    let to_list = |s: &str| -> Vec<T> {
        s.chars()
            .map(|c| {
                let i = (c as u32) - START;
                let list = if i % 2 == 0 { a } else { b };
                list[(i / 2) as usize].clone()
            })
            .collect()
    };

    dissimilar::diff(&sa, &sb)
        .into_iter()
        .map(|c| match c {
            dissimilar::Chunk::Equal(s) => ChunkT::Both(to_list(s)),
            dissimilar::Chunk::Delete(s) => ChunkT::Left(to_list(s)),
            dissimilar::Chunk::Insert(s) => ChunkT::Right(to_list(s)),
        })
        .collect()
}

/// Merge 2 lists. Keep changes from both sides.
pub fn merge_two_lists<T>(a: &[T], b: &[T]) -> Vec<T>
where
    T: Clone + Eq + Hash,
{
    diff_list(a, b)
        .into_iter()
        .flat_map(|c| match c {
            ChunkT::Both(v) => v,
            ChunkT::Left(v) => v,
            ChunkT::Right(v) => v,
        })
        .collect()
}

/// Merge characters in strings. Keep changes from both-side.
pub fn merge_two_strings_with_base(base: &str, a: &str, b: &str) -> String {
    let unchanged_a = unchanged_bytes(base, a);
    let unchanged_b = unchanged_bytes(base, b);
    // Mark part of a that is unchanged from `base`
    let chunks = dissimilar::diff(a, b);
    let mut result = Vec::with_capacity(chunks.len());
    let mut pos_a = 0;
    let mut pos_b = 0;
    for chunk in chunks {
        match chunk {
            dissimilar::Chunk::Equal(s) => {
                pos_a += s.len();
                pos_b += s.len();
                result.push(s);
            }
            dissimilar::Chunk::Delete(s) => {
                // Only in a.
                let next_pos_a = pos_a + s.len();
                if (pos_a..next_pos_a).all(|i| unchanged_a[i] == 1) {
                    // Same as "base". Deleted by b.
                } else {
                    result.push(s);
                }
                pos_a = next_pos_a;
            }
            dissimilar::Chunk::Insert(s) => {
                // Only in b.
                let next_pos_b = pos_b + s.len();
                if (pos_b..next_pos_b).all(|i| unchanged_b[i] == 1) {
                    // Same as "base". Deleted by a.
                } else {
                    result.push(s);
                }
                pos_b = next_pos_b;
            }
        }
    }
    result.concat()
}

/// Figure out parts of `new` that is not changed.
/// Return a vec, the i-th byte being 1 means "unchanged".
fn unchanged_bytes(old: &str, new: &str) -> Vec<u8> {
    let mut unchanged = vec![0u8; new.len()];
    let chunks = dissimilar::diff(old, new);
    let mut pos = 0;
    for chunk in chunks {
        match chunk {
            dissimilar::Chunk::Equal(s) => {
                let next_pos = pos + s.len();
                for i in pos..next_pos {
                    unchanged[i] = 1;
                }
                pos = next_pos;
            }
            dissimilar::Chunk::Delete(_) => {}
            dissimilar::Chunk::Insert(s) => {
                pos += s.len();
            }
        }
    }
    unchanged
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum ChunkT<T> {
    Both(T),
    Left(T),
    Right(T),
}

impl<'a> From<dissimilar::Chunk<'a>> for ChunkT<&'a str> {
    fn from(c: dissimilar::Chunk<'a>) -> Self {
        match c {
            dissimilar::Chunk::Equal(s) => ChunkT::Both(s),
            dissimilar::Chunk::Delete(s) => ChunkT::Left(s),
            dissimilar::Chunk::Insert(s) => ChunkT::Right(s),
        }
    }
}

impl<T> ChunkT<T> {
    /// Convert to `T`.
    fn pick_any(self) -> T {
        match self {
            ChunkT::Both(t) => t,
            ChunkT::Left(t) => t,
            ChunkT::Right(t) => t,
        }
    }

    fn pick_right_or(self, fallback: T) -> T {
        match self {
            ChunkT::Both(t) => t,
            ChunkT::Left(t) => fallback,
            ChunkT::Right(t) => t,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merge_two_lists() {
        let a = vec![1, 3, 2, 5];
        let b = vec![1, 3, 4, 2, 6, 5, 7];
        assert_eq!(merge_two_lists(&a, &b), [1, 3, 4, 2, 6, 5, 7]);
        assert_eq!(merge_two_lists(&b, &a), [1, 3, 4, 2, 6, 5, 7]);

        let a = vec![1, 2, 3, 5];
        let b = vec![1, 3, 4, 5];
        assert_eq!(merge_two_lists(&a, &b), [1, 2, 3, 4, 5]);
        assert_eq!(merge_two_lists(&b, &a), [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_merge_two_strings_with_base() {
        let base = "1 2 3 4";
        let t = |base, a, b| -> String { merge_two_strings_with_base(base, a, b) };
        // Delete by one side. Unchanged by the other.
        assert_eq!(t("1 2222 3 4", "1 3 4", "1 2222 4"), "1  4");
        // Insert by one side. Unchanged by the other.
        assert_eq!(t("1 3 5", "1 2222 3 5", "1 3 4 5"), "1 2222 33 4 5");
        // Insert by both sides. Different and same content.
        assert_eq!(t("1 3 5", "1 000 3 4 5", "1 22 3 4 5"), "1 00022 3 4 5");
        // Modified by both sides.
        assert_eq!(t("1 3 5", "1 000 5", "1 22 5"), "1 00022 5");
    }

    #[test]
    fn test_unchanged_bytes() {
        let old = "abcdefg";
        let new = "xabcydezzg";
        let b = unchanged_bytes(old, new);
        assert_eq!(b, &[0, 1, 1, 1, 0, 1, 1, 0, 0, 1]);
    }
}
