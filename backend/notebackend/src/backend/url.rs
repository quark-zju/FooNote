use super::Id;
use super::TreeBackend;
use std::io;
use std::io::Result;

/// Load a backend from a URL.
pub fn open(url: &str) -> Result<Box<dyn TreeBackend<Id = Id>>> {
    use super::blob;
    use std::path::Path;

    let foonote = "foonote:";
    let backend: Box<dyn TreeBackend<Id = Id>> = if url.starts_with(foonote) {
        let path = Path::new(&url[foonote.len()..]);
        Box::new(blob::SingleFileBackend::from_path(path)?.with_trash(true))
    } else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("invalid backend URL: {}", url),
        ));
    };

    Ok(backend)
}
