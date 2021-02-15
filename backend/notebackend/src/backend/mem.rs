use crate::backend::meta::blob::BlobBackend;
use crate::backend::meta::blob::BlobIo;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::io;
use std::sync::Arc;

#[derive(Clone)]
pub struct MemBlobIo {
    pub(crate) data: Arc<Vec<u8>>,
}

#[derive(Clone)]
pub struct NamedMemBlobIo {
    name: String,
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

static NAMED_MEMORY_BUFFERS: Lazy<Mutex<HashMap<String, Vec<u8>>>> = Lazy::new(Default::default);

impl BlobIo for NamedMemBlobIo {
    fn load(&mut self) -> std::io::Result<Box<dyn AsRef<[u8]>>> {
        let bytes = NAMED_MEMORY_BUFFERS
            .lock()
            .get(&self.name)
            .cloned()
            .unwrap_or_default();
        Ok(Box::new(bytes))
    }

    fn save(&mut self, data: Vec<u8>) -> std::io::Result<()> {
        NAMED_MEMORY_BUFFERS.lock().insert(self.name.clone(), data);
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

impl BlobBackend<NamedMemBlobIo> {
    pub fn from_named_memory(name: &str) -> io::Result<Self> {
        let blob_io = NamedMemBlobIo {
            name: name.to_string(),
        };
        Self::from_blob_io(blob_io)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::tests::*;
    use crate::manifest::TRASH_ID;
    use notebackend_types::TreeBackend;

    #[test]
    fn test_basic() {
        for &trash in &[false, true] {
            let mut backend = BlobBackend::empty().with_trash(trash);
            backend.check_generic().unwrap();
            backend.check_generic().unwrap();

            let bytes = backend.to_bytes();
            let backend2 = BlobBackend::from_bytes(bytes).unwrap().with_trash(trash);
            assert_eq!(backend.to_bytes(), backend2.to_bytes());
        }
    }

    #[test]
    fn test_named() {
        let mut backend = BlobBackend::from_named_memory("test_named_1").unwrap();
        backend.check_generic().unwrap();
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
                   \_ 6 ("F")
                   \_ 9 ("I")
                   \_ 8 ("H")
                   \_ 2 ("B")
                      \_ 3 ("C")
                      \_ 4 ("D")"#
        );

        let to_remove = backend.find_ids("A I E D H");
        backend.remove_batch(&to_remove).unwrap();
        assert_eq!(
            backend.draw_ascii(&ids),
            r#"
                root
                \_ 7 ("G")
                \_ 10 ("") (type=trash)
                   \_ 6 ("F")
                   \_ 2 ("B")
                   |  \_ 3 ("C")
                   \_ 1 ("A")
                   \_ 5 ("E")"#
        );
    }
}
