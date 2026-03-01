use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::ObjectTypes;

#[derive(Clone)]
pub struct Environment<'ctx> {
    store: HashMap<String, ObjectTypes<'ctx>>,
    outer: Option<Rc<RefCell<Environment<'ctx>>>>,
}

impl<'ctx> Environment<'ctx> {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment<'ctx>>>) -> Rc<RefCell<Self>> {
        let env = Self::new();
        env.borrow_mut().outer = Some(outer);
        env
    }

    pub fn get(&self, name: &str) -> Option<ObjectTypes<'ctx>> {
        let obj = self.store.get(name).cloned();
        if obj.is_none()
            && let Some(outer) = self.outer.as_ref()
        {
            return outer.borrow().get(name);
        }
        obj
    }

    pub fn set(&mut self, name: &str, val: ObjectTypes<'ctx>) {
        self.store.insert(name.to_string(), val);
    }
}
