use crate::backend::meta::blob::BlobBackend;
use crate::backend::meta::blob::BlobIo;
use fs2::FileExt;
use std::fs;
use std::io;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
pub struct FileBlobIo {
    path: PathBuf,

    // file might hold a lock.
    file: Option<fs::File>,
}

impl BlobIo for FileBlobIo {
    fn load(&mut self) -> std::io::Result<Box<dyn AsRef<[u8]>>> {
        match fs::OpenOptions::new().read(true).open(&self.path) {
            Ok(mut f) => {
                // Prevent loading a same file twice.
                f.try_lock_exclusive()?;
                let mut buf = Vec::new();
                f.read_to_end(&mut buf)?;
                self.file = Some(f);
                Ok(Box::new(buf))
            }
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    Ok(Box::new(b""))
                } else {
                    Err(e)
                }
            }
        }
    }

    fn save(&mut self, data: Vec<u8>) -> std::io::Result<()> {
        let mut file =
            tempfile::NamedTempFile::new_in(self.path.parent().unwrap_or_else(|| Path::new("")))?;
        // Close the existing file.
        self.file = None;
        file.as_file().try_lock_exclusive()?;
        file.write_all(&data)?;
        let file = file.persist(&self.path)?;
        self.file = Some(file);
        Ok(())
    }
}

impl BlobBackend<FileBlobIo> {
    /// Load from a given path.
    pub fn from_path(path: &Path) -> io::Result<Self> {
        let blob_io = FileBlobIo {
            path: path.to_path_buf(),
            file: None,
        };
        Self::from_blob_io(blob_io)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::tests::*;
    use notebackend_types::TreeBackend;

    #[test]
    fn test_basic() {
        for &trash in &[false, true] {
            let dir = tempfile::tempdir().unwrap();
            let path = dir.path().join("a");
            let mut backend = BlobBackend::from_path(&path).unwrap().with_trash(trash);
            backend.check_generic().unwrap();

            // Check persist.
            backend.remove_unreachable();
            let mut orig_data = backend.data.clone();
            orig_data.clear_mtime();
            backend.persist().unwrap();
            drop(backend); // Free the lock.

            let mut backend = BlobBackend::from_path(&path).unwrap().with_trash(trash);
            backend.remove_unreachable();
            let mut data = backend.data;
            data.clear_mtime();
            assert_eq!(orig_data, data);
        }
    }

    #[test]
    fn test_lock_exclusive() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("a");
        let mut backend = BlobBackend::from_path(&path).unwrap();
        backend.persist().unwrap();
        assert!(BlobBackend::from_path(&path).is_err());
        drop(backend);
        assert!(BlobBackend::from_path(&path).is_ok());

        let backend = BlobBackend::from_path(&path).unwrap();
        assert!(BlobBackend::from_path(&path).is_err());
    }
}
