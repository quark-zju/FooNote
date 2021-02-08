use crate::backend;
use notebackend_types::Id;
use notebackend_types::TreeBackend;
use std::io;
use std::{io::Result, path::Path};

/// Load a backend from a URL.
pub fn open(url: &str) -> Result<Box<dyn TreeBackend<Id = Id>>> {
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
        "git" => Ok(Box::new(backend::GitBackend::new(url, None)?)),
        "" if url.ends_with(".git") => Ok(Box::new(backend::GitBackend::new(url, None)?)),
        "foonote" | "" => Ok(Box::new(
            backend::SingleFileBackend::from_path(&Path::new(path))?.with_trash(true),
        )),
        "python" | "python-base64" => Ok(Box::new(backend::DylibBackend::open(
            "notebackend_python",
            url,
        )?)),
        _ if url.ends_with(".git") => Ok(Box::new(backend::GitBackend::new(url, None)?)),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("invalid backend URL: {}", url),
        )),
    }
}
