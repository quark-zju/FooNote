//! Blob backend. Backend that can be serialized into and deserialized from
//! a blob.

use crate::backend::meta::manifest::ManifestBasedBackend;
use crate::backend::meta::manifest::TextIO;
use crate::manifest::Manifest;
use notebackend_types::Id;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::io;
use std::io::Result;

/// How to read or write a blob. The blob contains full information about
/// a tree of notes.
pub trait BlobIo: Send + Sync + 'static {
    /// Load blob from backend.
    fn load(&mut self) -> io::Result<Box<dyn AsRef<[u8]>>>;

    /// Write data to backend.
    fn save(&mut self, data: Vec<u8>) -> io::Result<()>;
}

pub type BlobBackend<I> = ManifestBasedBackend<BlobTextIo<I>>;

/// Backend that loads and dumps from a single blob.
pub struct BlobTextIo<I> {
    pub(crate) blob_io: I,
    pub(crate) texts: BTreeMap<Id, String>,
    pub(crate) has_trash: bool,
}

impl<I: BlobIo> TextIO for BlobTextIo<I> {
    fn get_raw_text<'a>(&'a self, id: Id) -> Result<Cow<'a, str>> {
        let s = self.texts.get(&id).map(|s| s.as_str()).unwrap_or("");
        Ok(Cow::Borrowed(s))
    }

    fn set_raw_text(&mut self, id: Id, text: String) -> Result<()> {
        self.texts.insert(id, text);
        Ok(())
    }

    fn remove_raw_text(&mut self, id: Id) -> Result<()> {
        self.texts.remove(&id);
        Ok(())
    }

    fn persist_with_manifest(&mut self, manifest: &mut Manifest) -> Result<()> {
        let data = RefTreeData {
            texts: &self.texts,
            manifest: manifest,
        };
        let buf =
            serde_json::to_vec(&data).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        self.blob_io.save(buf)?;
        Ok(())
    }
}
#[derive(Serialize)]
struct RefTreeData<'a> {
    #[serde(rename = "notes")]
    texts: &'a BTreeMap<Id, String>,
    manifest: &'a Manifest,
}

#[derive(Deserialize, Default)]
struct TreeData {
    #[serde(default, rename = "notes")]
    texts: BTreeMap<Id, String>,
    #[serde(default)]
    manifest: Manifest,
}

impl<I: BlobIo> BlobBackend<I> {
    /// Remove unreachable nodes.
    pub fn remove_unreachable(&mut self) {
        let unreachable = self.manifest.remove_unreachable();
        for id in unreachable {
            self.text_io.texts.remove(&id);
        }
    }

    /// Converts to bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let data = self.to_serializable_data();
        let buf = serde_json::to_vec(&data).expect("serialize should succeed");
        buf
    }

    /// Convert to serializable data.
    fn to_serializable_data(&self) -> RefTreeData {
        RefTreeData {
            texts: &self.text_io.texts,
            manifest: &self.manifest,
        }
    }

    /// Construct from `BlobIo`.
    pub fn from_blob_io(mut blob_io: I) -> Result<Self> {
        let bytes = blob_io.load()?;
        let buf: &[u8] = (*bytes).as_ref();
        let mut data: TreeData = if buf.is_empty() {
            Default::default()
        } else {
            serde_json::from_slice(buf)?
        };
        data.manifest.rebuild_parents();
        let result = Self {
            text_io: BlobTextIo {
                blob_io,
                texts: data.texts,
                has_trash: false,
            },
            manifest: data.manifest,
        };
        Ok(result)
    }

    /// Enable or disable trash.
    pub fn with_trash(mut self, enabled: bool) -> Self {
        self.manifest = self.manifest.with_trash(enabled);
        self
    }
}
