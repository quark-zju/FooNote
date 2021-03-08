//! Null backend. Useful as "umounted" backend in a multiplex backend.

use crate::t;
use notebackend_types::Id;
use notebackend_types::InsertPos;
use notebackend_types::Mtime;
use notebackend_types::PersistCallbackFunc;
use notebackend_types::TreeBackend;
use std::borrow::Cow;
use std::io;
use std::io::Result;
pub struct NullBackend;

fn error<T>() -> Result<T> {
    Err(io::Error::new(
        io::ErrorKind::NotConnected,
        t!(cn = "无法修改已关闭节点", en = "backend was umounted",),
    ))
}

#[allow(unused_variables)]
impl TreeBackend for NullBackend {
    type Id = Id;

    fn get_children(&self, id: Id) -> Result<Vec<Id>> {
        Ok(Vec::new())
    }

    fn get_parent(&self, id: Id) -> Result<Option<Id>> {
        Ok(None)
    }

    fn get_mtime(&self, id: Id) -> Result<Mtime> {
        Ok(0)
    }

    fn get_text<'a>(&'a self, id: Id) -> Result<Cow<'a, str>> {
        Ok("".into())
    }

    fn get_raw_meta<'a>(&'a self, id: Id) -> Result<Cow<'a, str>> {
        Ok("".into())
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: InsertPos,
        text: String,
        meta: String,
    ) -> Result<Self::Id> {
        error()
    }

    fn set_parent(&mut self, id: Id, dest_id: Id, pos: InsertPos) -> Result<Id> {
        error()
    }

    fn set_text(&mut self, id: Id, text: String) -> Result<bool> {
        error()
    }

    fn set_raw_meta(&mut self, id: Id, content: String) -> Result<bool> {
        error()
    }

    fn remove(&mut self, id: Id) -> Result<()> {
        error()
    }

    fn persist(&mut self) -> Result<()> {
        Ok(())
    }

    fn persist_async(&mut self, callback: PersistCallbackFunc) {
        callback(Ok(()))
    }

    fn inline_data(&self) -> Option<&[u8]> {
        None
    }
}
