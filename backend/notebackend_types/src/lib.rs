//! Note Backend types (traits).
//!
//! This is a separate crate to be shared between the basic backend
//! implementation and specific backend implementations.

pub mod error;
pub mod meta;
pub mod tree;
pub mod util;

pub use self::meta::TreeMeta;
pub use tree::BackendId;
pub use tree::Id;
pub use tree::InsertPos;
pub use tree::Mtime;
pub use tree::TreeBackend;

/// Function to construct a backend. Useful for Rust shared libraries.
pub type CreateBackendFunc = fn(url: &str) -> std::io::Result<Box<dyn TreeBackend<Id = Id>>>;

pub use tree::PersistCallbackFunc;
