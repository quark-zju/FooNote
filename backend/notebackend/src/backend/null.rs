//! Null backend. Useful as "umounted" backend in a multiplex backend.

use super::Id;
use super::InsertPos;
use super::TreeBackend;
use std::borrow::Cow;
use std::io;
use std::io::Result;
pub struct NullBackend;

fn error<T>() -> Result<T> {
    Err(io::Error::new(
        io::ErrorKind::NotConnected,
        "backend was umounted",
    ))
}

#[allow(unused_variables)]
impl TreeBackend for NullBackend {
    type Id = Id;

    fn get_children(&self, id: Id) -> Result<Vec<Id>> {
        error()
    }

    fn get_parent(&self, id: Id) -> Result<Option<Id>> {
        error()
    }

    fn get_mtime(&self, id: Id) -> Result<super::Mtime> {
        error()
    }

    fn get_text<'a>(&'a self, id: Id) -> Result<Cow<'a, str>> {
        error()
    }

    fn get_raw_meta<'a>(&'a self, id: Id) -> Result<Cow<'a, str>> {
        error()
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: super::InsertPos,
        text: String,
        meta: String,
    ) -> Result<Self::Id> {
        error()
    }

    fn set_parent(&mut self, id: Id, dest_id: Id, pos: InsertPos) -> Result<Id> {
        error()
    }

    fn set_text(&mut self, id: Id, text: String) -> Result<()> {
        error()
    }

    fn set_raw_meta(&mut self, id: Id, content: String) -> Result<()> {
        error()
    }

    fn remove(&mut self, id: Id) -> Result<()> {
        error()
    }

    fn persist(&mut self) -> Result<()> {
        Ok(())
    }
}
