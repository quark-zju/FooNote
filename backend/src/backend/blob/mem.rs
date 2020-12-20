use super::BlobBackend;
use super::BlobIo;
use std::io;
use std::sync::Arc;

#[derive(Clone)]
pub struct MemBlobIo {
    data: Arc<Vec<u8>>,
}

impl AsRef<[u8]> for MemBlobIo {
    fn as_ref(&self) -> &[u8] {
        self.data.as_ref()
    }
}

impl BlobIo for MemBlobIo {
    fn load(&mut self) -> std::io::Result<Box<dyn AsRef<[u8]>>> {
        Ok(Box::new(self.clone()))
    }

    fn save(&mut self, data: Vec<u8>) -> std::io::Result<()> {
        self.data = Arc::new(data);
        Ok(())
    }
}

impl BlobBackend<MemBlobIo> {
    /// Load from a given path.
    pub fn from_bytes(data: Vec<u8>) -> io::Result<Self> {
        let data = Arc::new(data);
        let blob_io = MemBlobIo { data };
        Self::from_blob_io(blob_io)
    }

    /// Create an empty in-memory backend.
    pub fn empty() -> Self {
        Self::from_bytes(Vec::new()).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::super::TRASH_ID;
    use super::*;
    use crate::backend::tests::*;
    use crate::backend::TreeBackend;

    #[test]
    fn test_basic() {
        for &trash in &[false, true] {
            let mut backend = BlobBackend::empty().with_trash(trash);
            backend.check_generic().unwrap();

            let bytes = backend.to_bytes();
            let backend2 = BlobBackend::from_bytes(bytes).unwrap().with_trash(trash);
            assert_eq!(backend.to_bytes(), backend2.to_bytes());
        }
    }

    #[test]
    fn test_trash() {
        let mut backend = BlobBackend::empty().with_trash(true);
        backend.insert_ascii(
            r"
            A--B--C  E--F  G--H
                \           \
                 D           I",
        );
        let mut ids = backend.find_ids("root A B C D E F G H I");
        assert_eq!(
            backend.draw_ascii(&ids),
            r#"
                root
                \_ 1 ("A")
                |  \_ 2 ("B")
                |     \_ 3 ("C")
                |     \_ 4 ("D")
                \_ 5 ("E")
                |  \_ 6 ("F")
                \_ 7 ("G")
                   \_ 8 ("H")
                   \_ 9 ("I")"#
        );

        let to_remove = backend.find_ids("F I H C B");
        backend.remove_batch(&to_remove).unwrap();
        assert!(to_remove
            .iter()
            .all(|i| backend.is_ancestor(TRASH_ID, *i).unwrap()));

        ids.push(TRASH_ID);
        assert_eq!(
            backend.draw_ascii(&ids),
            r#"
                root
                \_ 1 ("A")
                \_ 5 ("E")
                \_ 7 ("G")
                \_ 10 ("") (type=trash)
                   \_ 2 ("B")
                   |  \_ 3 ("C")
                   |  \_ 4 ("D")
                   \_ 6 ("F")
                   \_ 8 ("H")
                   \_ 9 ("I")"#
        );

        let to_remove = backend.find_ids("A I E D H");
        backend.remove_batch(&to_remove).unwrap();
        assert_eq!(
            backend.draw_ascii(&ids),
            r#"
                root
                \_ 7 ("G")
                \_ 10 ("") (type=trash)
                   \_ 2 ("B")
                   |  \_ 3 ("C")
                   \_ 6 ("F")
                   \_ 1 ("A")
                   \_ 5 ("E")"#
        );
    }
}
