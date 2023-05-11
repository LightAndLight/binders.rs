use std::collections::HashSet;

use super::name::Name;
use super::permutation::Permutable;

pub trait Supported: Permutable {
    fn support(&self) -> HashSet<Name>;
}

impl Supported for Name {
    fn support(&self) -> HashSet<Name> {
        HashSet::from([*self])
    }
}

impl<T: Eq + std::hash::Hash + Supported> Supported for HashSet<T> {
    fn support(&self) -> HashSet<Name> {
        self.iter().fold(HashSet::new(), |mut support, item| {
            support.extend(item.support());
            support
        })
    }
}

impl<T: Supported> Supported for Box<T> {
    fn support(&self) -> HashSet<Name> {
        self.as_ref().support()
    }
}
