use crate::backend::meta::blob::BlobBackend;
use crate::backend::meta::blob::BlobFormat;
use crate::backend::meta::blob::BlobIo;
use crate::t;
use ::sha1::{Digest, Sha1};
use aes_gcm_siv::aead::{generic_array::GenericArray, Aead, NewAead};
use aes_gcm_siv::Aes256GcmSiv;
use notebackend_types::log;
use rand::RngCore;
use scrypt::Params as ScryptParams;
use std::convert::TryInto;
use std::io;

const SALT_LEN: usize = 32;
const NONCE_LEN: usize = 12;
const SHA1_LEN: usize = 20;
pub struct Aes256BlobIo {
    // salt + iv + data
    data: Vec<u8>,
    key: Box<Key>,

    // used to avoid re-encrypt same content
    last_input_sha1: [u8; SHA1_LEN],
}

struct Key([u8; 32]);

impl Drop for Key {
    fn drop(&mut self) {
        self.0 = rand::random();
    }
}

impl Aes256BlobIo {
    fn salt(&self) -> [u8; SALT_LEN] {
        self.data[..SALT_LEN].try_into().unwrap()
    }

    fn nonce(&mut self) -> [u8; NONCE_LEN] {
        let result = self.data[SALT_LEN..(SALT_LEN + NONCE_LEN)]
            .try_into()
            .unwrap();
        log::trace!("nonce: {:?}", &result);
        result
    }

    fn next_nonce(&mut self, sha1: [u8; SHA1_LEN]) -> [u8; NONCE_LEN] {
        // IV = IV + 1
        let mut i = SALT_LEN;
        loop {
            self.data[i] = self.data[i].wrapping_add(1);
            if self.data[i] == 0 && i < SALT_LEN + NONCE_LEN {
                i += 1;
            } else {
                break;
            }
        }
        // IV = IV xor SHA1. Attempt to avoid IV reuse.
        for i in 1..NONCE_LEN {
            let i = i + SALT_LEN;
            if self.data.len() < i {
                self.data[i] ^= sha1[i];
            }
        }
        self.nonce()
    }

    fn cipher(&self) -> Aes256GcmSiv {
        let key = GenericArray::from_slice(&self.key.0);
        let cipher = Aes256GcmSiv::new(key);
        cipher
    }
}

impl BlobIo for Aes256BlobIo {
    fn load(&mut self) -> io::Result<Box<dyn AsRef<[u8]>>> {
        // Decrypt
        let cipher = self.cipher();
        let nonce = self.nonce();
        let nonce = GenericArray::from_slice(&nonce);
        let encrypted = self.data.get((SALT_LEN + NONCE_LEN)..).unwrap_or(b"");
        let text = if encrypted.is_empty() {
            Vec::<u8>::new()
        } else {
            cipher.decrypt(nonce, encrypted).map_err(|e| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    t!(cn = "解密失败: {}", en = "Cannot decrypt: {}", e),
                )
            })?
        };
        self.last_input_sha1 = sha1(&text);
        Ok(Box::new(text))
    }

    fn save(&mut self, data: Vec<u8>) -> io::Result<()> {
        let sha1 = sha1(&data);
        if sha1 == self.last_input_sha1 {
            log::debug!("no change - skip re-encryption");
            return Ok(());
        }

        // Encrypt (to inline_data())
        let nonce = self.next_nonce(sha1);
        let mut result = self.salt().to_vec();
        result.extend_from_slice(&nonce);
        let nonce = GenericArray::from_slice(&nonce);
        let cipher = self.cipher();
        let encrypted = cipher.encrypt(nonce, &data[..]).expect("encrypt failure");
        log::debug!("encrypted {} into {} bytes", data.len(), encrypted.len());
        result.extend(encrypted);
        self.data = result;
        self.last_input_sha1 = sha1;
        Ok(())
    }

    fn inline_data(&self) -> Option<&[u8]> {
        Some(&self.data)
    }

    fn format() -> BlobFormat {
        BlobFormat::CBOR
    }
}

impl BlobBackend<Aes256BlobIo> {
    /// Load from a given path.
    pub fn from_encrypted_bytes(mut data: Vec<u8>, password: &str) -> io::Result<Self> {
        // Add salt if data is empty.
        if data.is_empty() {
            let mut salt: [u8; SALT_LEN + NONCE_LEN] = [0u8; SALT_LEN + NONCE_LEN];
            let mut rng = rand::thread_rng();
            rng.fill_bytes(&mut salt);
            log::debug!("new random salt + nonce: {}", base64::encode(&salt));
            data.extend(&salt);
        }
        let salt = match data.get(..SALT_LEN) {
            None => return Err(io::ErrorKind::InvalidData.into()),
            Some(salt) => salt,
        };
        log::trace!("salt: {}", base64::encode(&salt));

        // Produce key.
        let key = password_derive_key(&salt, password);
        let blob_io = Aes256BlobIo {
            data,
            key,
            last_input_sha1: Default::default(),
        };
        Self::from_blob_io(blob_io)
    }
}

fn sha1(data: &[u8]) -> [u8; SHA1_LEN] {
    let mut hasher = Sha1::new();
    hasher.update(data);
    hasher.finalize().into()
}

/// Derive key from password.
fn password_derive_key(salt: &[u8], password: &str) -> Box<Key> {
    let params = if cfg!(test) {
        ScryptParams::new(1, 1, 1).unwrap()
    } else {
        ScryptParams::new(15, 8, 1).unwrap()
    };
    let mut output = [0u8; 32];
    scrypt::scrypt(password.as_bytes(), salt, &params, &mut output).unwrap();
    Box::new(Key(output))
}
