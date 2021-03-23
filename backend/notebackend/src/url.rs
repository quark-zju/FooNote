use crate::backend;
use crate::t;
use notebackend_types::log;
use notebackend_types::Id;
use notebackend_types::TreeBackend;
use once_cell::sync::Lazy;
use std::fmt;
use std::io;
use std::path::PathBuf;
use std::{io::Result, path::Path};

/// Load a backend from a URL.
pub fn open(url: &str, inline_data: Option<&[u8]>) -> Result<Box<dyn TreeBackend<Id = Id>>> {
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
            let mut backend: Box<dyn TreeBackend<Id = Id>> = {
                let name = url
                    .strip_prefix("memory:")
                    .and_then(|name| name.split(";").filter(|s| !s.contains('=')).next());
                if let Some(name) = name {
                    Box::new(backend::NamedMemBackend::from_named_memory(name)?)
                } else {
                    let data = inline_data.map(|d| d.to_vec()).unwrap_or_default();
                    Box::new(backend::MemBackend::from_bytes(data)?)
                }
            };
            #[cfg(test)]
            {
                use crate::backend::tests::TestTreeBackend;
                if let Some(ascii) = url.strip_prefix("memory:ascii=") {
                    backend.insert_ascii(ascii);
                }
            }
            backend
        }
        BackendType::Python => Box::new(backend::DylibBackend::open(
            "notebackend_python",
            url,
            inline_data,
        )?),
        BackendType::Aes256 => {
            // Wrong API usage.
            return Err(io::Error::new(
                io::ErrorKind::AddrNotAvailable,
                t!(cn = "AES256 需提供密码", en = "AES256 requires password"),
            ));
        }
    };

    Ok(backend)
}

pub fn open_aes256(password: &str, data: Vec<u8>) -> io::Result<Box<dyn TreeBackend<Id = Id>>> {
    log::info!("open aes256: {}", str::repeat("*", password.len()));
    #[cfg(feature = "encrypt")]
    {
        let backend = crate::backend::Aes256Backend::from_encrypted_bytes(data, password)?;
        return Ok(Box::new(backend));
    }

    #[allow(unreachable_code)]
    Err(io::Error::new(
        io::ErrorKind::AddrNotAvailable,
        t!(
            cn = "AES256 功能未编译",
            en = "AES256 feature was not compiled"
        ),
    ))
}

/// Find out the backend type from a URL.
pub fn backend_type_from_url(url: &str) -> io::Result<BackendType> {
    let backend_type = if url.ends_with(".git") || url.ends_with(".git/") {
        BackendType::Git
    } else if url.ends_with(".foonote") {
        BackendType::Local
    } else if url == "aes256" || url.starts_with("aes256:") {
        BackendType::Aes256
    } else if url == "memory" || url.starts_with("memory:") {
        BackendType::Memory
    } else if url.ends_with(".py")
        || url
            .splitn(2, ' ')
            .next()
            .unwrap_or_default()
            .ends_with(".py")
    {
        BackendType::Python
    } else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            t!(cn = "无效地址：{}", en = "invalid backend URL: {}", url),
        ));
    };
    Ok(backend_type)
}

/// Type of a backend.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum BackendType {
    Git,
    Local,
    Memory,
    Python,
    Aes256,
}

impl fmt::Display for BackendType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            BackendType::Git => "git",
            BackendType::Local => "foonote",
            BackendType::Memory => "memory",
            BackendType::Python => "python",
            BackendType::Aes256 => "aes256",
        };
        f.write_str(name)
    }
}

/// "Current directory" on Windows (if writable). Or the "App Dir".
pub(crate) static APP_DIR: Lazy<PathBuf> = Lazy::new(|| {
    // Can we use "cwd"? Only try this on Windows (Portable Software).
    #[cfg(windows)]
    if let Ok(cwd) = std::env::current_dir() {
        // If cwd is "/" (started by macOS Finder), do not attempt to use
        // it as the application dir.
        if cwd.display().to_string() != "/" {
            // Can we write to cwd?
            if tempfile::tempfile_in(&cwd).is_ok() {
                log::info!("AppDir: {} (cwd)", cwd.display());
                return cwd;
            }
        }
    }

    // Use user "data dir".
    let path = match dirs::data_dir() {
        None => std::path::Path::new(".").to_path_buf(),
        Some(p) => p,
    }
    .join("foonote");
    log::info!("AppDir: {} (data dir)", path.display());
    let _ = std::fs::create_dir_all(&path);
    path
});
