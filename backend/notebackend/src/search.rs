use notebackend_types::TreeBackend;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Acquire;
use std::sync::atomic::Ordering::Release;
use std::sync::Arc;
use std::thread;

enum State {
    /// The tree does not have desired content.
    None,
    /// The tree has desired content.
    Some,
}

pub struct Search<T: TreeBackend> {
    backend: Arc<RwLock<T>>,
    aborted: Arc<AtomicBool>,
    completed: Arc<AtomicBool>,

    input: String,
    roots: Vec<T::Id>,
    result: Arc<RwLock<Vec<(T::Id, String)>>>,
}

// Workaround #[derive(Clone)] not effective.
impl<T: TreeBackend> Clone for Search<T> {
    fn clone(&self) -> Self {
        Self {
            backend: self.backend.clone(),
            aborted: self.aborted.clone(),
            completed: self.completed.clone(),
            input: self.input.clone(),
            roots: self.roots.clone(),
            result: self.result.clone(),
        }
    }
}

impl<T> Search<T>
where
    T: TreeBackend,
{
    pub fn new(backend: Arc<RwLock<T>>) -> Self {
        Self {
            backend,
            aborted: Default::default(),
            completed: Arc::new(AtomicBool::new(true)),
            input: Default::default(),
            roots: Default::default(),
            result: Default::default(),
        }
    }

    pub fn start(&mut self, input: String, roots: Vec<T::Id>) {
        if &input == &self.input && &roots == &self.roots {
            // Already searching for the same thing.
            return;
        }

        // Start a new search. Abort the existing one.
        self.stop();

        self.roots = roots;
        self.input = input;
        self.aborted = Default::default();
        self.completed = Default::default();
        self.result = Default::default();
        let this: Search<T> = self.clone();
        thread::spawn(move || this.run_search());
    }

    pub fn stop(&self) {
        self.aborted.store(true, Release);
    }

    fn run_search(&self) {
        let mut to_visit: Vec<T::Id> = self.roots.clone();
        let visited: HashMap<T::Id, bool> = Default::default();
        while let Some(id) = to_visit.pop() {
            if self.aborted.load(Acquire) {
                break;
            }
            let text = {
                let backend = self.backend.read();
                // Copy text to unlock.
                backend.get_text(id).map(|t| t.to_string()).ok()
            };
            if let Some(text) = text {
                for line in text.lines() {
                    if line.contains(&self.input) {
                        let line = line.to_string();
                        self.result.write().push((id, line));
                        break;
                    }
                }
            }
            if let Ok(children) = self.backend.read().get_children(id) {
                to_visit.extend(children.into_iter().rev());
            }
        }
        self.completed.store(true, Release);
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn result<'a>(&'a self) -> impl Deref<Target = Vec<(T::Id, String)>> + 'a {
        self.result.read()
    }

    pub fn is_completed(&self) -> bool {
        self.completed.load(Acquire)
    }
}

impl<T> Drop for Search<T>
where
    T: TreeBackend,
{
    fn drop(&mut self) {
        self.aborted.store(true, Release);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::tests::TestTreeBackend;
    use crate::backend::MemBackend;
    use crate::backend::MultiplexBackend;

    fn check_search(backend: &Arc<RwLock<MultiplexBackend>>, input: &str, expected: &str) {
        let mut s = Search::new(backend.clone());
        s.start(input.to_string(), vec![backend.read().get_root_id()]);
        while !s.is_completed() {
            thread::sleep(std::time::Duration::from_millis(5));
        }

        {
            let result = s.result().clone();
            let backend = backend.read();
            let expected: Vec<_> = expected
                .split_whitespace()
                .map(|s| (backend.find(s), s.to_string()))
                .collect();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_search_basic() {
        let mut b = MultiplexBackend::from_root_backend(Box::new(MemBackend::empty()));
        b.insert_ascii(
            r"
            Ax---By---Cx
                  \
                   Dxy",
        );
        let b = Arc::new(RwLock::new(b));
        check_search(&b, "x", "Ax Cx Dxy");
        check_search(&b, "y", "By Dxy");
    }
}
