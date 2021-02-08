use std::fmt;

mod file;
mod mem;

use crate::backend::meta::blob::BlobBackend;

pub type SingleFileBackend = BlobBackend<file::FileBlobIo>;
pub type MemBackend = BlobBackend<mem::MemBlobIo>;

impl PartialEq for MemBackend {
    fn eq(&self, other: &Self) -> bool {
        self.blob_io.data == other.blob_io.data && self.has_trash == other.has_trash
    }
}

impl fmt::Debug for MemBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("MemBackend")?;
        self.blob_io.data.fmt(f)?;
        Ok(())
    }
}
