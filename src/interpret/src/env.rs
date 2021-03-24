use crate::object::Object;
use parser::Identifier;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    pub fn get(&self, id: &Identifier) -> Option<&Object> {
        self.store.get(&id.name)
    }

    pub fn set(&mut self, id: &Identifier, obj: Object) -> Option<Object> {
        self.store.insert(id.name.clone(), obj)
    }
}
