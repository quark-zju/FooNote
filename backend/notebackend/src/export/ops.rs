//! APis on actual backends.

use super::errno;
use super::stack;
use crate::backend;
use crate::backend::FullId;
use crate::backend::MultiplexBackend;
use crate::clipboard;
use crate::lang::LangId;
use crate::search::Search;
use backend::MemBackend;
use log::Level::{Debug, Error, Info, Trace, Warn};
use notebackend_types::log;
use notebackend_types::InsertPos;
use notebackend_types::TreeBackend;
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use parking_lot::RwLockUpgradableReadGuard;
use std::env;
use std::io;
use std::sync::Arc;
use std::sync::Mutex as StdMutex;

/// Global state for easier ffi APIs.
static ROOT_BACKEND: Lazy<Arc<RwLock<MultiplexBackend>>> = Lazy::new(|| Default::default());
static SEARCH: Lazy<RwLock<Search<MultiplexBackend>>> =
    Lazy::new(|| RwLock::new(Search::new(ROOT_BACKEND.clone())));

static PERSIST_RESULT_LIST: Lazy<RwLock<Vec<Arc<StdMutex<Option<io::Result<()>>>>>>> =
    Lazy::new(|| Default::default());

macro_rules! pop {
    () => {
        match stack::pop() {
            Err(i) => return i,
            Ok(v) => v,
        }
    };
}

macro_rules! pop_fid {
    () => {{
        let id: ::notebackend_types::Id = match stack::pop() {
            Err(i) => return i,
            Ok(v) => v,
        };
        let backend_id: ::notebackend_types::BackendId = match stack::pop() {
            Err(i) => return i,
            Ok(v) => v,
        };
        (backend_id, id)
    }};
}

macro_rules! pop_fid_list {
    () => {{
        let len: usize = pop!();
        let mut ids = Vec::with_capacity(len);
        for _ in 0..len {
            ids.push(pop_fid!());
        }
        ids
    }};
}

macro_rules! push_return {
    ($r:expr) => {
        match $r {
            Ok(v) => {
                stack::push(v);
                errno::OK
            }
            Err(e) => {
                stack::push(e.to_string());
                errno::from_io_error(&e)
            }
        }
    };
}

macro_rules! nop_return {
    ($r:expr) => {
        match $r {
            Ok(()) => errno::OK,
            Err(e) => {
                stack::push(e.to_string());
                errno::from_io_error(&e)
            }
        }
    };
}

macro_rules! attempt {
    ($r:expr) => {
        match $r {
            Ok(v) => v,
            Err(e) => {
                stack::push(e.to_string());
                return errno::from_io_error(&e);
            }
        }
    };
}

fn push_fid(id: FullId) {
    stack::push(id.0);
    stack::push(id.1);
}

fn push_fid_list(ids: &[FullId]) {
    for &id in ids.iter().rev() {
        push_fid(id);
    }
    stack::push(ids.len());
}

/// (url: String) -> (backend_id: i32, id: i32)
#[no_mangle]
pub extern "C" fn notebackend_open_root_url() -> i32 {
    let url: String = pop!();
    let m = attempt!(MultiplexBackend::open_url(&url));
    push_fid(m.get_root_id());
    *ROOT_BACKEND.write() = m;
    errno::OK
}

/// (url: String) -> (backend_type: str)
#[no_mangle]
pub extern "C" fn notebackend_type_of_url() -> i32 {
    let url: String = pop!();
    let backend_type = attempt!(crate::url::backend_type_from_url(&url));
    stack::push(backend_type.to_string());
    errno::OK
}

/// () -> ()
#[no_mangle]
pub extern "C" fn notebackend_close_all() {
    let m = MultiplexBackend::default();
    *ROOT_BACKEND.write() = m;
}

/// () -> (backend_id: i32, id: i32)
#[no_mangle]
pub extern "C" fn notebackend_get_root_id() -> i32 {
    push_fid(ROOT_BACKEND.read().get_root_id());
    errno::OK
}

/// (backend_id: i32, id: i32) -> ([child: (i32, i32)], count: i32)
#[no_mangle]
pub extern "C" fn notebackend_get_children() -> i32 {
    let fid: FullId = pop_fid!();
    let children = attempt!(ROOT_BACKEND.read().get_children(fid));
    for &i in children.iter() {
        push_fid(i)
    }
    stack::push(children.len());
    errno::OK
}

/// (backend_id: i32, id: i32) -> (backend_id: i32, id: i32)
#[no_mangle]
pub extern "C" fn notebackend_get_parent() -> i32 {
    let id = pop_fid!();
    let parent_id = attempt!(ROOT_BACKEND.read().get_parent(id));
    push_fid(parent_id.unwrap_or(id));
    errno::OK
}

/// (backend_id: i32, id: i32) -> (mtime: i32)
#[no_mangle]
pub extern "C" fn notebackend_get_mtime() -> i32 {
    let id = pop_fid!();
    push_return!(ROOT_BACKEND.read().get_mtime(id))
}

/// (backend_id: i32, id: i32) -> (text: String)
#[no_mangle]
pub extern "C" fn notebackend_get_text() -> i32 {
    let id = pop_fid!();
    push_return!(ROOT_BACKEND.read().get_text(id).map(|s| s.to_string()))
}

/// (backend_id: i32, id: i32) -> (text: String)
#[no_mangle]
pub extern "C" fn notebackend_get_text_first_line() -> i32 {
    let id = pop_fid!();
    push_return!(ROOT_BACKEND.read().get_text_first_line(id))
}

/// (backend_id: i32, id: i32) -> (meta: String)
#[no_mangle]
pub extern "C" fn notebackend_get_raw_meta() -> i32 {
    let id = pop_fid!();
    push_return!(ROOT_BACKEND.read().get_raw_meta(id).map(|s| s.to_string()))
}

/// (backend_id: i32, id: i32, prefix: String) -> (value: String)
#[no_mangle]
pub extern "C" fn notebackend_extract_meta() -> i32 {
    let prefix: String = pop!();
    let id = pop_fid!();
    push_return!(ROOT_BACKEND
        .read()
        .extract_meta(id, &prefix)
        .map(|s| s.to_string()))
}

/// (dest: (i32, i32), pos: i32, text: String, meta: String) -> (backend_id: i32, id: i32)
#[no_mangle]
pub extern "C" fn notebackend_insert() -> i32 {
    let meta: String = pop!();
    let text: String = pop!();
    let pos: i32 = pop!();
    let pos: InsertPos = pos.into();
    let dest = pop_fid!();
    let id = attempt!(ROOT_BACKEND.write().insert(dest, pos, text, meta));
    push_fid(id);
    errno::OK
}

/// (id: (i32, i32), parent_id: (i32, i32), index: i32) -> (id: (i32, i32))
#[no_mangle]
pub extern "C" fn notebackend_set_parent() -> i32 {
    let pos: i32 = pop!();
    let dest_id = pop_fid!();
    let id = pop_fid!();
    let id = attempt!(ROOT_BACKEND.write().set_parent(id, dest_id, pos.into()));
    push_fid(id);
    errno::OK
}

/// ([id: (i32, i32)], count: i32, parent_id: (i32, i32), index: i32) -> ([id: (i32, i32)], count: i32)
#[no_mangle]
pub extern "C" fn notebackend_set_parent_batch() -> i32 {
    let pos: i32 = pop!();
    let dest_id = pop_fid!();
    let ids = pop_fid_list!();
    let ids = attempt!(ROOT_BACKEND
        .write()
        .set_parent_batch(&ids, dest_id, pos.into()));
    push_fid_list(&ids);
    errno::OK
}

/// (backend_id: i32, id: i32, text: String) -> ()
#[no_mangle]
pub extern "C" fn notebackend_set_text() -> i32 {
    let text: String = pop!();
    let id = pop_fid!();
    nop_return!(ROOT_BACKEND.write().set_text(id, text).map(|_| ()))
}

/// (backend_id: i32, id: i32, meta: String) -> ()
#[no_mangle]
pub extern "C" fn notebackend_set_raw_meta() -> i32 {
    let meta: String = pop!();
    let id = pop_fid!();
    nop_return!(ROOT_BACKEND.write().set_raw_meta(id, meta).map(|_| ()))
}

/// (backend_id: i32, id: i32, prefix: String, value: String) -> ()
#[no_mangle]
pub extern "C" fn notebackend_update_meta() -> i32 {
    let value: String = pop!();
    let prefix: String = pop!();
    let id = pop_fid!();
    nop_return!(ROOT_BACKEND
        .write()
        .update_meta(id, &prefix, &value)
        .map(|_| ()))
}

/// (backend_id: i32, id: i32) -> ()
#[no_mangle]
pub extern "C" fn notebackend_remove() -> i32 {
    let id = pop_fid!();
    nop_return!(ROOT_BACKEND.write().remove(id))
}

/// ([id: (i32, i32)], len: i32) -> ()
#[no_mangle]
pub extern "C" fn notebackend_remove_batch() -> i32 {
    let ids = pop_fid_list!();
    nop_return!(ROOT_BACKEND.write().remove_batch(&ids))
}

/// (backend_id: i32, id: i32) -> ()
/// Try to autofill an empty node by checking pattern from previous ids.
#[no_mangle]
pub extern "C" fn notebackend_autofill() -> i32 {
    let id = pop_fid!();
    nop_return!(ROOT_BACKEND.write().autofill(id))
}

/// () -> ()
#[no_mangle]
pub extern "C" fn notebackend_persist() -> i32 {
    nop_return!(ROOT_BACKEND.write().persist())
}

/// () -> ()
/// Potentially spawns a thread to do the "slow" part of "persist".
/// The result can be obtained by notebackend_persist_try_wait().
#[no_mangle]
pub extern "C" fn notebackend_persist_async() -> i32 {
    let list = PERSIST_RESULT_LIST.upgradable_read();
    if !list.is_empty() {
        // Prevent multiple threads. This is just some protection to avoid
        // overload the system.
        stack::push(
            "persist thread is already running - use notebackend_persist_try_wait()".to_string(),
        );
        return errno::EWOULDBLOCK;
    }

    let mut list = RwLockUpgradableReadGuard::upgrade(list);
    let result = Arc::new(StdMutex::new(None));
    list.push(result.clone());
    ROOT_BACKEND.write().persist_async({
        let result = result.clone();
        Box::new(move |r| *result.lock().unwrap() = Some(r))
    });
    errno::OK
}

/// () -> (message: str, errno: int)
/// - EINVAL: No pending thread. Call notebackend_persist_async().
/// - EWOULDBLOCK: Result is not ready yet.
/// - OK: Result is returned as `errno`.
#[no_mangle]
pub extern "C" fn notebackend_persist_try_wait() -> i32 {
    let last = PERSIST_RESULT_LIST.write().pop();
    if let Some(last) = last {
        let code = match last.try_lock() {
            Ok(result) => match &*result {
                None => {
                    stack::push("persist thread is still running".to_string());
                    errno::EWOULDBLOCK
                }
                Some(r) => {
                    let (message, errno) = match r {
                        Err(e) => (e.to_string(), errno::from_io_error(&e)),
                        Ok(_) => ("".to_string(), errno::OK),
                    };
                    stack::push(message);
                    stack::push(errno);
                    errno::OK
                }
            },
            Err(_) => {
                stack::push("persist thread is still running".to_string());
                errno::EWOULDBLOCK
            }
        };
        if code != errno::OK {
            // Push back to the waiting list.
            PERSIST_RESULT_LIST.write().push(last);
        }
        code
    } else {
        stack::push("persist thread is not running - use notebackend_persist_async()".to_string());
        errno::EINVAL
    }
}

/// ([id: (i32, i32)], count: i32) -> (bytes: bytes)
/// Copy selected items to bytes.
#[no_mangle]
pub extern "C" fn notebackend_copy() -> i32 {
    let ids = pop_fid_list!();
    let copied = {
        let backend = ROOT_BACKEND.read();
        attempt!(clipboard::copy(&*backend, &ids))
    };
    stack::push(copied.to_bytes());
    errno::OK
}

/// (dest_id: (i32, i32), pos: i32, bytes: bytes) -> ([new_id: (i32, i32)], len: i32)
/// Paste from copied bytes.
#[no_mangle]
pub extern "C" fn notebackend_paste() -> i32 {
    eprintln!("paste");
    let bytes = pop!();
    let copied = attempt!(MemBackend::from_bytes(bytes));

    let pos: i32 = pop!();
    let dest = pop_fid!();

    let mut backend = ROOT_BACKEND.write();
    let ids = attempt!(clipboard::paste(&copied, &mut *backend, dest, pos.into()));
    let ids = attempt!(backend.get_heads(&ids));
    push_fid_list(&ids);
    errno::OK
}

/// ([id: (i32, i32)], count: i32) -> ([id: (i32, i32)], count: i32)
#[no_mangle]
pub extern "C" fn notebackend_get_heads() -> i32 {
    let ids = pop_fid_list!();
    let heads = attempt!(ROOT_BACKEND.read().get_heads(&ids));
    push_fid_list(&heads);
    errno::OK
}

/// (text: str, [id: (i32, i32)], id_count: i32) -> ()
/// Search from given roots.
#[no_mangle]
pub extern "C" fn notebackend_search_start() -> i32 {
    let roots = pop_fid_list!();
    let text: String = pop!();
    SEARCH.write().start(text, roots);
    errno::OK
}

/// () -> ()
#[no_mangle]
pub extern "C" fn notebackend_search_stop() -> i32 {
    SEARCH.write().stop();
    errno::OK
}

/// (skip: i32) -> ([(id: (i32, i32), line: str)], count: i32)
#[no_mangle]
pub extern "C" fn notebackend_search_result() -> i32 {
    let skip = pop!();
    let search = SEARCH.read();
    let result = search.result();
    let mut count = 0;
    for (id, line) in result.iter().skip(skip).rev() {
        push_fid(*id);
        stack::push(line.to_string());
        count += 1;
    }
    stack::push(count);
    errno::OK
}

/// () -> (completed_bool: i32)
#[no_mangle]
pub extern "C" fn notebackend_search_is_complete() -> i32 {
    let completed = SEARCH.read().is_completed();
    stack::push(completed as i32);
    errno::OK
}

/// () -> (input: str)
#[no_mangle]
pub extern "C" fn notebackend_search_input() -> i32 {
    let input = SEARCH.read().input().to_string();
    stack::push(input);
    errno::OK
}

/// (str) -> ()
/// Set the current language.
#[no_mangle]
pub extern "C" fn notebackend_set_lang() -> i32 {
    let name: String = pop!();
    let name = name.to_lowercase();
    let lang_id: LangId = if name.contains("cn") || name.contains("zh") || name.contains("ch") {
        LangId::Cn
    } else {
        LangId::En
    };
    // safety: writing the u8 is atomic.
    unsafe { crate::lang::CURRENT_LANG_ID = lang_id };
    errno::OK
}

/// () -> (str)
/// Get the current language ("cn", "en").
#[no_mangle]
pub extern "C" fn notebackend_get_lang() -> i32 {
    // safety: reading the u8 is atomic.
    let lang_id = unsafe { crate::lang::CURRENT_LANG_ID };
    stack::push(lang_id.to_string());
    errno::OK
}

/// () -> ()
#[no_mangle]
pub extern "C" fn notebackend_enable_env_logger() -> i32 {
    match env_logger::builder()
        .format_timestamp_millis()
        .parse_env("FOONOTE_LOG")
        .try_init()
    {
        Ok(_) => errno::OK,
        Err(e) => {
            stack::push(e.to_string());
            errno::EIO
        }
    }
}

/// Log target for the ffi.
const TARGET: &str = "frontend";

/// () -> ()
/// Return the current max log level.
#[no_mangle]
pub extern "C" fn notebackend_log_max_level() -> i32 {
    if log::log_enabled!(target: TARGET, Trace) {
        5
    } else if log::log_enabled!(target: TARGET, Debug) {
        4
    } else if log::log_enabled!(target: TARGET, Info) {
        3
    } else if log::log_enabled!(target: TARGET, Warn) {
        2
    } else if log::log_enabled!(target: TARGET, Error) {
        1
    } else {
        0
    }
}

// Exported "log". Set both "target" and "module" to TARGET.
macro_rules! export_log {
    ($lvl:expr, $($arg:tt)+) => ({
        let lvl = $lvl;
        if lvl <= log::STATIC_MAX_LEVEL && lvl <= log::max_level() {
            log::__private_api_log(
                log::__log_format_args!($($arg)+),
                lvl,
                &(TARGET, TARGET, log::__log_file!(), log::__log_line!()),
            );
        }
    });
}

#[no_mangle]
pub extern "C" fn notebackend_log_error() {
    if let Ok(s) = stack::pop::<String>() {
        export_log!(Error, "{}", s)
    }
}

#[no_mangle]
pub extern "C" fn notebackend_log_warn() {
    if let Ok(s) = stack::pop::<String>() {
        export_log!(Warn, "{}", s)
    }
}

#[no_mangle]
pub extern "C" fn notebackend_log_info() {
    if let Ok(s) = stack::pop::<String>() {
        export_log!(Info, "{}", s)
    }
}

#[no_mangle]
pub extern "C" fn notebackend_log_debug() {
    if let Ok(s) = stack::pop::<String>() {
        export_log!(Debug, "{}", s)
    }
}

#[no_mangle]
pub extern "C" fn notebackend_log_trace() {
    if let Ok(s) = stack::pop::<String>() {
        export_log!(Trace, "{}", s)
    }
}

// () -> (str)
// Obtain the git version.
#[no_mangle]
pub extern "C" fn notebackend_git_desc() -> i32 {
    // NOTEBACKEND_GIT_DESC is set by build.rs.
    let desc = env!("NOTEBACKEND_GIT_DESC");
    stack::push(desc.to_string());
    errno::OK
}

// () -> (str)
// Obtain the directory for storing application data.
#[no_mangle]
pub extern "C" fn notebackend_app_dir() -> i32 {
    let path = crate::url::APP_DIR.display().to_string();
    stack::push(path);
    errno::OK
}
