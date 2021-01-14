use notebackend_types::CreateBackendFunc;
use notebackend_types::Id;
use notebackend_types::TreeBackend;
use pyo3::{prelude::*, types::PyDict};
use std::fs;
use std::io;

pub fn notebackend_create(url: &str) -> io::Result<Box<dyn TreeBackend<Id = Id>>> {
    let split = url.splitn(2, ':').collect::<Vec<_>>();
    if let [scheme, path] = split[..] {
        let code = if scheme == "python" {
            fs::read_to_string(path)?
        } else if scheme == "python-base64" {
            let decoded = base64::decode(path.to_string().as_bytes())
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
            String::from_utf8_lossy(&decoded).to_string()
        } else {
            return unsupported(url);
        };
        let gil = Python::acquire_gil();
        let py = gil.python();
        let globals = py.eval("globals().copy()", None, None)?;
        let scope = globals.cast_as::<PyDict>().unwrap();
        py.run(&code, Some(scope), None).map_err(map_err)?;
        if let Some(get_instance) = scope.get_item("get_instance") {
            let arg = path.splitn(2, ':').nth(1).unwrap_or_default();
            let instance = get_instance.call1((arg,)).map_err(map_err)?;
            let instance = instance.to_object(py);
            let backend = PythonBackend { instance };
            Ok(Box::new(backend))
        } else {
            python_error("get_instance(path) does not exist")
        }
    } else {
        unsupported(url)
    }
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

fn map_err<E>(e: E) -> io::Error
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    io::Error::new(io::ErrorKind::Other, e)
}

fn with_gil<F, R>(func: F) -> R
where
    F: FnOnce(Python) -> R,
{
    let gil = Python::acquire_gil();
    let py = gil.python();
    func(py)
}

struct PythonBackend {
    instance: PyObject,
}

impl TreeBackend for PythonBackend {
    type Id = Id;

    fn get_children(&self, id: Self::Id) -> io::Result<Vec<Self::Id>> {
        with_gil(|py| {
            let children = self.instance.call_method1(py, "get_children", (id,))?;
            Ok(children.extract(py)?)
        })
    }

    fn get_parent(&self, id: Self::Id) -> io::Result<Option<Self::Id>> {
        with_gil(|py| {
            let parent = self.instance.call_method1(py, "get_parent", (id,))?;
            Ok(parent.extract(py)?)
        })
    }

    fn get_mtime(&self, id: Self::Id) -> io::Result<notebackend_types::Mtime> {
        with_gil(|py| {
            let result = self.instance.call_method1(py, "get_mtime", (id,))?;
            Ok(result.extract(py)?)
        })
    }

    fn get_text<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        with_gil(|py| {
            let result = self.instance.call_method1(py, "get_text", (id,))?;
            Ok(result.extract::<String>(py)?)
        })
        .map(Into::into)
    }

    fn get_raw_meta<'a>(&'a self, id: Self::Id) -> io::Result<std::borrow::Cow<'a, str>> {
        with_gil(|py| {
            let result = self.instance.call_method1(py, "get_raw_meta", (id,))?;
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
        with_gil(|py| {
            let result =
                self.instance
                    .call_method1(py, "insert", (dest_id, i32::from(pos), text, meta))?;
            Ok(result.extract::<Id>(py)?)
        })
    }

    fn set_parent(
        &mut self,
        id: Self::Id,
        dest_id: Self::Id,
        pos: notebackend_types::InsertPos,
    ) -> io::Result<Self::Id> {
        with_gil(|py| {
            let result =
                self.instance
                    .call_method1(py, "set_parent", (id, dest_id, i32::from(pos)))?;
            Ok(result.extract::<Id>(py)?)
        })
    }

    fn set_text(&mut self, id: Self::Id, text: String) -> io::Result<()> {
        with_gil(move |py| {
            self.instance.call_method1(py, "set_text", (id, text))?;
            Ok(())
        })
    }

    fn set_raw_meta(&mut self, id: Self::Id, content: String) -> io::Result<()> {
        with_gil(move |py| {
            self.instance
                .call_method1(py, "set_raw_meta", (id, content))?;
            Ok(())
        })
    }

    fn remove(&mut self, id: Self::Id) -> io::Result<()> {
        with_gil(move |py| {
            self.instance.call_method1(py, "remove", (id,))?;
            Ok(())
        })
    }

    fn persist(&mut self) -> io::Result<()> {
        with_gil(move |py| {
            self.instance.call_method0(py, "persist")?;
            Ok(())
        })
    }
}
