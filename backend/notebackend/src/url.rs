use crate::backend;
use notebackend_types::Id;
use notebackend_types::TreeBackend;
use std::fmt;
use std::io;
use std::{io::Result, path::Path};

/// Load a backend from a URL.
pub fn open(url: &str) -> Result<Box<dyn TreeBackend<Id = Id>>> {
    log::info!("open url: {}", url);

    let backend: Box<dyn TreeBackend<Id = Id>> = match backend_type_from_url(url)? {
        BackendType::Git => {
            Box::new(backend::GitBackend::from_git_url(url, None)?.with_trash(true))
        }
        BackendType::Local => {
            Box::new(backend::SingleFileBackend::from_path(&Path::new(url))?.with_trash(true))
        }
        BackendType::Memory => {
            #[allow(unused_mut)]
            let mut backend = backend::MemBackend::empty();
            #[cfg(test)]
            {
                use crate::backend::tests::TestTreeBackend;
                if let Some(ascii) = url.strip_prefix("memory:ascii=") {
                    backend.insert_ascii(ascii);
                }
            }
            Box::new(backend)
        }
        BackendType::Python => Box::new(backend::DylibBackend::open("notebackend_python", url)?),
    };

    Ok(backend)
}

/// Find out the backend type from a URL.
pub fn backend_type_from_url(url: &str) -> io::Result<BackendType> {
    let backend_type = if url.ends_with(".git") || url.ends_with(".git/") {
        BackendType::Git
    } else if url.ends_with(".foonote") {
        BackendType::Local
    } else if url == "memory" || url.starts_with("memory:") {
        BackendType::Memory
    } else if url.ends_with(".py") {
        BackendType::Python
    } else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("invalid backend URL: {}", url),
        ));
    };
    Ok(backend_type)
}

/// Type of a backend.
#[derive(Copy, Clone)]
pub enum BackendType {
    Git,
    Local,
    Memory,
    Python,
}

impl fmt::Display for BackendType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            BackendType::Git => "git",
            BackendType::Local => "foonote",
            BackendType::Memory => "memory",
            BackendType::Python => "python",
        };
        f.write_str(name)
    }
}
