//! Blob backend. Backend that can be serialized into and deserialized from
//! a blob.

use crate::backend::meta::manifest::ManifestBasedBackend;
use crate::backend::meta::manifest::TextIO;
use crate::manifest::CompactManifest;
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

    /// Inlined data.
    fn inline_data(&self) -> Option<&[u8]> {
        None
    }

    /// True: CBOR; False: JSON.
    fn format() -> BlobFormat {
        BlobFormat::JSON
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BlobFormat {
    JSON,
    CBOR,
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
        let buf = match I::format() {
            BlobFormat::JSON => {
                let data = RefTreeData {
                    texts: &self.texts,
                    manifest: &manifest,
                };
                serde_json::to_vec(&data).expect("serialize should succeed")
            }
            BlobFormat::CBOR => {
                let data = CompactRefTreeData {
                    texts: &self.texts,
                    manifest: manifest.to_compact(),
                };
                serde_cbor::to_vec(&data).expect("serialize should succeed")
            }
        };
        self.blob_io.save(buf)?;
        Ok(())
    }

    fn get_inline_data(&self) -> Option<&[u8]> {
        self.blob_io.inline_data()
    }
}
#[derive(Serialize)]
struct RefTreeData<'a> {
    #[serde(rename = "notes", alias = "t")]
    texts: &'a BTreeMap<Id, String>,
    #[serde(rename = "manifest", alias = "m")]
    manifest: &'a Manifest,
}

#[derive(Serialize)]
struct CompactRefTreeData<'a> {
    #[serde(alias = "notes", rename = "t")]
    texts: &'a BTreeMap<Id, String>,
    #[serde(alias = "manifest", rename = "m")]
    manifest: CompactManifest<'a>,
}

#[derive(Deserialize, Default)]
struct TreeData {
    #[serde(default, rename = "notes", alias = "t")]
    texts: BTreeMap<Id, String>,
    #[serde(default, rename = "manifest", alias = "m")]
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
        let buf = match I::format() {
            BlobFormat::JSON => {
                let data = RefTreeData {
                    texts: &self.text_io.texts,
                    manifest: &self.manifest,
                };
                serde_json::to_vec(&data).expect("serialize should succeed")
            }
            BlobFormat::CBOR => {
                let data = CompactRefTreeData {
                    texts: &self.text_io.texts,
                    manifest: self.manifest.to_compact(),
                };
                serde_cbor::to_vec(&data).expect("serialize should succeed")
            }
        };
        buf
    }

    /// Construct from `BlobIo`.
    pub fn from_blob_io(mut blob_io: I) -> Result<Self> {
        let bytes = blob_io.load()?;
        let buf: &[u8] = (*bytes).as_ref();
        let mut data: TreeData = if buf.is_empty() {
            Default::default()
        } else {
            match I::format() {
                BlobFormat::JSON => serde_json::from_slice(buf)?,
                BlobFormat::CBOR => serde_cbor::from_slice(buf)
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
            }
        };
        data.manifest.rebuild_parents();
        let result = Self::from_manifest_text_io(
            data.manifest,
            BlobTextIo {
                blob_io,
                texts: data.texts,
                has_trash: false,
            },
        );
        Ok(result)
    }
}
