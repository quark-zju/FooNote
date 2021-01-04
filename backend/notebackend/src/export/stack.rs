//! Stack used to exchange data between Rust and Pascal.
//! The stack owns data, making it easier to pass owned data.
//! For example, instead of returning a Rust String to Pascal,
//! where Pascal does not know how to deal with it, the stack
//! allows Rust to return references to Pascal, then Pascal
//! can copy the data, and then pop the item on the stack.

use super::errno::ENONE;
use super::errno::ETYPE;
use super::errno::OK;
use super::Variant;
use std::cell::RefCell;
use std::convert::TryInto;

thread_local! {
    static STACK: RefCell<Vec<Variant>> = Default::default();
}

pub(crate) fn clear() {
    STACK.with(|s| s.borrow_mut().clear())
}

pub(crate) fn len() -> u32 {
    STACK.with(|s| s.borrow_mut().len() as _)
}

pub(crate) fn push<T: Into<Variant>>(v: T) {
    STACK.with(|s| s.borrow_mut().push(v.into()));
}

pub(crate) fn pop<T>() -> Result<T, i32>
where
    Variant: TryInto<T>,
{
    STACK.with(|s| match s.borrow_mut().pop() {
        None => Err(ENONE),
        Some(v) => {
            let t: Result<T, _> = v.try_into();
            match t {
                Err(_) => Err(ETYPE),
                Ok(v) => Ok(v),
            }
        }
    })
}

/// Remove everything in the thread-local stack.
#[no_mangle]
pub extern "C" fn notebackend_stack_clear() {
    clear();
}

/// Obtain the number of items in the thread-local stack.
#[no_mangle]
pub extern "C" fn notebackend_stack_len() -> u32 {
    len()
}

/// Push a 32-bit signed integer to the thread-local stack.
#[no_mangle]
pub extern "C" fn notebackend_stack_push_i32(value: i32) {
    push(value)
}

/// Push bytes to the thread-local stack.
/// Copy the slice so the foreign caller can release it.
#[no_mangle]
pub extern "C" fn notebackend_stack_push_bytes(offset: *const u8, size: usize) {
    let slice = unsafe { std::slice::from_raw_parts(offset, size) };
    push(slice.to_vec())
}

/// Push string in utf8 bytes to the thread-local stack.
/// Copy the slice so the foreign caller can release it.
#[no_mangle]
pub extern "C" fn notebackend_stack_push_str(offset: *const u8, size: usize) {
    let slice = unsafe { std::slice::from_raw_parts(offset, size) };
    let string = String::from_utf8_lossy(slice).to_string();
    push(string)
}

/// Read the last item of the thread-local stack, as an i32 integer.
/// Return OK on success.
#[no_mangle]
pub extern "C" fn notebackend_stack_last_i32(ret: *mut i32) -> i32 {
    STACK.with(|s| match s.borrow().last() {
        None => ENONE,
        Some(Variant::Int(value)) => unsafe {
            *ret = *value;
            OK
        },
        Some(_) => ETYPE,
    })
}

/// Read the last item of the thread-local stack, as bytes.
/// Return OK on success.
#[no_mangle]
pub extern "C" fn notebackend_stack_last_bytes(offset: *mut *const u8, size: *mut usize) -> i32 {
    STACK.with(|s| match s.borrow().last() {
        None => ENONE,
        Some(Variant::Bytes(bytes)) => unsafe {
            *offset = bytes.as_ptr();
            *size = bytes.len();
            OK
        },
        Some(_) => ETYPE,
    })
}

/// Read the last item of the thread-local stack, as string.
/// Return OK on success.
#[no_mangle]
pub extern "C" fn notebackend_stack_last_str(offset: *mut *const u8, size: *mut usize) -> i32 {
    STACK.with(|s| match s.borrow().last() {
        None => ENONE,
        Some(Variant::Str(string)) => unsafe {
            *offset = string.as_bytes().as_ptr();
            *size = string.as_bytes().len();
            OK
        },
        Some(_) => ETYPE,
    })
}

/// Remove the last item in the thread-local stack.
#[no_mangle]
pub extern "C" fn notebackend_stack_pop() {
    STACK.with(|s| s.borrow_mut().pop());
}

/// Print the content of the thread-local stack for debugging purpose.
#[no_mangle]
pub extern "C" fn notebackend_stack_debug() {
    STACK.with(|s| eprintln!("{:#?}", s.borrow()));
}
