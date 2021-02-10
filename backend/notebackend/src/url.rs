use crate::backend;
use notebackend_types::Id;
use notebackend_types::TreeBackend;
use std::io;
use std::{io::Result, path::Path};

/// Load a backend from a URL.
pub fn open(url: &str) -> Result<Box<dyn TreeBackend<Id = Id>>> {
    log::info!("open url: {}", url);

    let b: Box<dyn TreeBackend<Id = Id>> = if url.ends_with(".git") || url.ends_with(".git/") {
        Box::new(backend::GitBackend::from_git_url(url, None)?.with_trash(true))
    } else if url.ends_with(".foonote") {
        Box::new(backend::SingleFileBackend::from_path(&Path::new(url))?.with_trash(true))
    } else if url == "memory" {
        Box::new(backend::GitBackend::from_git_url(url, None)?.with_trash(true))
    } else if url.ends_with(".py") {
        Box::new(backend::DylibBackend::open("notebackend_python", url)?)
    } else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("invalid backend URL: {}", url),
        ));
    };

    Ok(b)
}
