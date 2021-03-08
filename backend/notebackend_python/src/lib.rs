use cpython::{NoArgs, ObjectProtocol, PyDict, PyErr, PyObject, PyResult, Python};
use notebackend_types::CreateBackendFunc;
use notebackend_types::Id;
use notebackend_types::PersistCallbackFunc;
use notebackend_types::TreeBackend;
use std::fs;
use std::io;

#[no_mangle]
pub fn notebackend_create(
    url: &str,
    _inline_data: Option<&[u8]>,
) -> io::Result<Box<dyn TreeBackend<Id = Id>>> {
    let split = url.splitn(2, ' ').collect::<Vec<_>>();
    let (path, arg) = match split[..] {
        [path, arg] => (path, arg),
        [path] => (path, ""),
        _ => return unsupported(url),
    };
    let code = fs::read_to_string(path)?;

    with_gil(|py| -> io::Result<Box<dyn TreeBackend<Id = Id>>> {
        let globals = py.eval("globals().copy()", None, None).map_err(map_pyerr)?;
        let scope = globals.extract::<PyDict>(py).map_err(map_pyerr)?;
        py.run(&code, Some(&scope), None).map_err(map_pyerr)?;
        if let Some(get_instance) = scope.get_item(py, "get_instance") {
            let instance = get_instance.call(py, (arg,), None).map_err(map_pyerr)?;
            let backend = PythonBackend { instance };
            Ok(Box::new(backend))
        } else {
            python_error("get_instance(path) does not exist")
        }
    })
}

// Ensure the function signature matches.
const _: CreateBackendFunc = notebackend_create;

fn python_error<T>(message: &str) -> io::Result<T> {
    notebackend_types::error::invalid_input(message)
}

fn unsupported<T>(url: &str) -> io::Result<T> {
    notebackend_types::error::invalid_input(&format!(
        "url {} is not supported by the Python backend",
        url
    ))
}

fn map_pyerr(e: PyErr) -> io::Error {
    let message = (|| -> PyResult<String> {
        with_gil(|py| {
            let locals = PyDict::new(py);
            locals.set_item(py, "tb", &e.ptraceback)?;
            locals.set_item(py, "ty", &e.ptype)?;
            locals.set_item(py, "e", &e.pvalue)?;
            let msg = py
                .eval(
                    "''.join(__import__('traceback').format_exception(ty,(isinstance(e,ty) and e or ty(e)),tb))",
                    None,
                    Some(&locals),
                )?;
            msg.extract(py)
        })
    })().unwrap_or_else(|_| format!("{:?}", e));
    io::Error::new(io::ErrorKind::Other, message)
}

fn with_gil<F, R>(func: F) -> R
where
    F: FnOnce(Python) -> R,
{
    let gil = Python::acquire_gil();
    let py = gil.python();
    func(py)
}

fn with_gil_map_err<F, T>(func: F) -> io::Result<T>
where
    F: FnOnce(Python) -> PyResult<T>,
{
    let gil = Python::acquire_gil();
    let py = gil.python();
    func(py).map_err(map_pyerr)
}

struct PythonBackend {
    instance: PyObject,
}

impl TreeBackend for PythonBackend {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> io::Result<Vec<Self::Id>> {
        with_gil_map_err(|py| {
            let children = self.instance.call_method(py, "get_children", (id,), None)?;
            Ok(children.extract(py)?)
        })
    }

    fn get_parent(&self, id: Self::Id) -> io::Result<Option<Self::Id>> {
        with_gil_map_err(|py| {
            let parent = self.instance.call_method(py, "get_parent", (id,), None)?;
            Ok(parent.extract(py)?)
        })
    }

    fn get_mtime(&self, id: Self::Id) -> io::Result<notebackend_types::Mtime> {
        with_gil_map_err(|py| {
            let result = self.instance.call_method(py, "get_mtime", (id,), None)?;
            Ok(result.extract(py)?)
        })
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        with_gil_map_err(|py| {
            let result = self.instance.call_method(py, "get_text", (id,), None)?;
            Ok(result.extract::<String>(py)?)
        })
        .map(Into::into)
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        with_gil_map_err(|py| {
            let result = self.instance.call_method(py, "get_raw_meta", (id,), None)?;
            Ok(result.extract::<String>(py)?)
        })
        .map(Into::into)
    }

    fn insert(
        &mut self,
        dest_id: Self::Id,
        pos: notebackend_types::InsertPos,
        text: String,
        meta: String,
    ) -> io::Result<Self::Id> {
        with_gil_map_err(|py| {
            let result = self.instance.call_method(
                py,
                "insert",
                (dest_id, i32::from(pos), text, meta),
                None,
            )?;
            Ok(result.extract::<Id>(py)?)
        })
    }

    fn set_parent(
        &mut self,
        id: Self::Id,
        dest_id: Self::Id,
        pos: notebackend_types::InsertPos,
    ) -> io::Result<Self::Id> {
        with_gil_map_err(|py| {
            let result =
                self.instance
                    .call_method(py, "set_parent", (id, dest_id, i32::from(pos)), None)?;
            Ok(result.extract::<Id>(py)?)
        })
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> io::Result<bool> {
        with_gil_map_err(move |py| {
            let result = self
                .instance
                .call_method(py, "set_text", (id, text), None)?;
            Ok(result.extract::<bool>(py)?)
        })
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> io::Result<bool> {
        with_gil_map_err(move |py| {
            let result = self
                .instance
                .call_method(py, "set_raw_meta", (id, content), None)?;
            Ok(result.extract::<bool>(py)?)
        })
    }

    fn remove(&mut self, id: Self::Id) -> io::Result<()> {
        with_gil_map_err(move |py| {
            self.instance.call_method(py, "remove", (id,), None)?;
            Ok(())
        })
    }

    fn persist(&mut self) -> io::Result<()> {
        with_gil_map_err(move |py| {
            self.instance.call_method(py, "persist", NoArgs, None)?;
            Ok(())
        })
    }

    fn inline_data(&self) -> Option<&[u8]> {
        None
    }

    fn persist_async(&mut self, callback: PersistCallbackFunc) {
        let r = self.persist();
        callback(r);
    }
}
