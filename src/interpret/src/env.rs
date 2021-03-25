use crate::object::Object;
use crate::EnvPointer;
use parser::Identifier;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

#[derive(Debug, Default, Clone)]
pub struct Env {
    store: HashMap<String, Rc<Object>>,
    outer: RefCell<Weak<RefCell<Env>>>,
}

impl PartialEq for Env {
    fn eq(&self, other: &Env) -> bool {
        self.store == other.store
    }
}

impl Eq for Env {}

impl Env {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn add_outer(&mut self, env: EnvPointer) {
        *self.outer.borrow_mut() = Rc::downgrade(&env);
    }

    pub fn get(&self, id: &Identifier) -> Option<Rc<Object>> {
        match self.outer.borrow().upgrade() {
            None => self.store.get(&id.name).cloned(),
            Some(env) => {
                if let Some(obj) = self.store.get(&id.name) {
                    Some(Rc::clone(&obj))
                } else {
                    env.borrow().get(id)
                }
            }
        }
    }

    pub fn set(&mut self, id: &Identifier, obj: Rc<Object>) -> Option<Rc<Object>> {
        match self.outer.borrow().upgrade() {
            None => self.store.insert(id.name.clone(), obj),
            Some(env) => env.borrow_mut().set(id, obj),
        }
        // self.store.insert(id.name.clone(), obj)
    }
}
