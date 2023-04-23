use std::collections::HashMap;

use crate::name::Name;

pub trait Context {
    type Item;
    fn insert(&mut self, key: Name, value: Self::Item) -> Option<Self::Item>;
    fn get(&self, key: &Name) -> Option<&Self::Item>;
    fn remove(&mut self, key: &Name) -> Option<Self::Item>;

    fn with<R>(&mut self, key: Name, value: Self::Item, f: impl FnOnce(&mut Self) -> R) -> R {
        self.insert(key, value);
        let result = f(self);
        self.remove(&key);
        result
    }
}

impl<A> Context for HashMap<Name, A> {
    type Item = A;

    fn insert(&mut self, key: Name, value: Self::Item) -> Option<A> {
        self.insert(key, value)
    }

    fn get(&self, key: &Name) -> Option<&A> {
        self.get(key)
    }

    fn remove(&mut self, key: &Name) -> Option<A> {
        self.remove(key)
    }
}
