#![allow(dead_code)]
#![allow(unused_variables)]

/// (Reusable) metadata of backends.
mod manifest;

/// Note backend implementations.
mod backend;

/// Clipboard utilities.
mod clipboard;

/// Search support for backends.
mod search;

/// URL -> Backend.
mod url;

/// Merge algorithms.
mod merge;

/// i18 related utilities.
mod lang;

/// Exported FFI.
mod export;
