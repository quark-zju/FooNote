use crate::backend;
use notebackend_types::Id;
use notebackend_types::TreeBackend;
use std::io;
use std::{io::Result, path::Path};

/// Load a backend from a URL.
pub fn open(url: &str) -> Result<Box<dyn TreeBackend<Id = Id>>> {
    log::info!("open url: {}", url);
    let (scheme, path) = {
        let mut split = url.splitn(2, ':');
        let segment1 = split.next().unwrap_or_default();
        let segment2 = split.next().unwrap_or_default();
        if segment2.is_empty() {
            ("", url)
        } else {
            (segment1, segment2)
        }
    };

    match scheme {
        "git" => Ok(Box::new(
            backend::GitBackend::from_git_url(url, None)?.with_trash(true),
        )),
        "foonote" => Ok(Box::new(
            backend::SingleFileBackend::from_path(&Path::new(path))?.with_trash(true),
        )),
        "python" | "python-base64" => Ok(Box::new(backend::DylibBackend::open(
            "notebackend_python",
            url,
        )?)),
        "memory" => Ok(Box::new(backend::MemBackend::empty())),
        _ if url.ends_with(".git") => Ok(Box::new(
            backend::GitBackend::from_git_url(url, None)?.with_trash(true),
        )),
        "" => Ok(Box::new(
            backend::SingleFileBackend::from_path(&Path::new(path))?.with_trash(true),
        )),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("invalid backend URL: {}", url),
        )),
    }
}
