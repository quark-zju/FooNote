use std::io;

pub(crate) const OK: i32 = 0;
pub(crate) const ENONE: i32 = -1;
pub(crate) const ETYPE: i32 = -2;
pub(crate) const EIO: i32 = -3;
pub(crate) const EINVAL: i32 = -4;

pub(crate) fn from_io_error(e: &io::Error) -> i32 {
    use io::ErrorKind::*;
    match e.kind() {
        InvalidInput => EINVAL,
        InvalidData => EINVAL,
        _ => EIO,
    }
}
