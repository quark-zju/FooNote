use std::io::Error;
use std::io::ErrorKind;
use std::io::Result;

pub fn invalid_data<T>(message: &str) -> Result<T> {
    Err(Error::new(ErrorKind::InvalidData, message))
}

pub fn invalid_input<T>(message: &str) -> Result<T> {
    Err(Error::new(ErrorKind::InvalidData, message))
}
