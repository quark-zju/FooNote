use super::blob;
use super::dylib;
use super::git;
use super::Id;
use super::TreeBackend;
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
        "git" => Ok(Box::new(git::GitBackend::new(url, None)?)),
        "ssh" | "http" | "https" | "" if url.ends_with(".git") => {
            Ok(Box::new(git::GitBackend::new(url, None)?))
        }
        "foonote" | "" => Ok(Box::new(
            blob::SingleFileBackend::from_path(&Path::new(path))?.with_trash(true),
        )),
        "python" | "python-base64" => Ok(Box::new(dylib::DylibBackend::open(
            "notebackend_python",
            url,
        )?)),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("invalid backend URL: {}", url),
        )),
    }
}
