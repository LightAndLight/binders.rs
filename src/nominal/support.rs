use std::collections::HashSet;

use super::name::Name;

pub trait Support {
    fn support(&self) -> HashSet<Name>;
}

impl Support for Name {
    fn support(&self) -> HashSet<Name> {
        HashSet::from([*self])
    }
}

impl<T: Eq + std::hash::Hash + Support> Support for HashSet<T> {
    fn support(&self) -> HashSet<Name> {
        self.iter().fold(HashSet::new(), |mut support, item| {
            support.extend(item.support());
            support
        })
    }
}
