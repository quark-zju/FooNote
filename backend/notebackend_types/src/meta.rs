use crate::TreeBackend;
use std::io::Result;
pub trait TreeMeta: TreeBackend {
    fn is_copyable(&self, id: Self::Id) -> Result<bool> {
        Ok(self.extract_meta(id, "copyable=")? != "false")
    }

    fn is_pinned(&self, id: Self::Id) -> Result<bool> {
        Ok(self.extract_meta(id, "pin=")? != "false")
    }
}

impl<T: TreeBackend> TreeMeta for T {}
