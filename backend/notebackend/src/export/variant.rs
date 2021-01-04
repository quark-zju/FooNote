use std::convert::TryInto;
/// A variant of a primitive value. Used in `stack.rs` to store values
/// for ffi input / output.
#[derive(Debug)]
pub(crate) enum Variant {
    Bytes(Box<[u8]>),
    Str(String),
    Int(i32),
}

impl From<String> for Variant {
    fn from(s: String) -> Self {
        Variant::Str(s)
    }
}

impl From<i32> for Variant {
    fn from(i: i32) -> Self {
        Variant::Int(i)
    }
}

impl From<usize> for Variant {
    fn from(i: usize) -> Self {
        Variant::Int(i as _)
    }
}

impl From<Vec<u8>> for Variant {
    fn from(b: Vec<u8>) -> Self {
        b.into_boxed_slice().into()
    }
}

impl From<Box<[u8]>> for Variant {
    fn from(b: Box<[u8]>) -> Self {
        Variant::Bytes(b)
    }
}

impl<'a> TryInto<&'a str> for &'a Variant {
    type Error = ();

    fn try_into(self) -> Result<&'a str, Self::Error> {
        match self {
            Variant::Str(s) => Ok(s.as_str()),
            _ => Err(()),
        }
    }
}

impl<'a> TryInto<&'a [u8]> for &'a Variant {
    type Error = ();

    fn try_into(self) -> Result<&'a [u8], Self::Error> {
        match self {
            Variant::Bytes(b) => Ok(b.as_ref()),
            _ => Err(()),
        }
    }
}

impl<'a> TryInto<i32> for &'a Variant {
    type Error = ();

    fn try_into(self) -> Result<i32, Self::Error> {
        match self {
            Variant::Int(i) => Ok(*i),
            _ => Err(()),
        }
    }
}

impl TryInto<String> for Variant {
    type Error = ();

    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Variant::Str(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl TryInto<Vec<u8>> for Variant {
    type Error = ();

    fn try_into(self) -> Result<Vec<u8>, Self::Error> {
        match self {
            Variant::Bytes(b) => Ok(b.into_vec()),
            _ => Err(()),
        }
    }
}

impl TryInto<Box<[u8]>> for Variant {
    type Error = ();

    fn try_into(self) -> Result<Box<[u8]>, Self::Error> {
        match self {
            Variant::Bytes(b) => Ok(b),
            _ => Err(()),
        }
    }
}

impl TryInto<i32> for Variant {
    type Error = ();

    fn try_into(self) -> Result<i32, Self::Error> {
        match self {
            Variant::Int(i) => Ok(i),
            _ => Err(()),
        }
    }
}

impl TryInto<usize> for Variant {
    type Error = ();

    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            Variant::Int(i) => Ok(i as _),
            _ => Err(()),
        }
    }
}

impl TryInto<isize> for Variant {
    type Error = ();

    fn try_into(self) -> Result<isize, Self::Error> {
        match self {
            Variant::Int(i) => Ok(i as _),
            _ => Err(()),
        }
    }
}
