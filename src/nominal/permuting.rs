use std::{borrow::Cow, collections::HashSet};

use super::{
    name::Name,
    permutation::{Permutation, Permute},
    support::Support,
};

pub struct Permuting<'a, T> {
    pub permutation: Cow<'a, Permutation>,
    pub value: T,
}

impl<'a, T> From<T> for Permuting<'a, T> {
    fn from(value: T) -> Self {
        Permuting {
            permutation: Cow::Owned(Permutation::id()),
            value,
        }
    }
}

impl<'a, T: Permute> Permute for Permuting<'a, T> {
    fn permute_mut(&mut self, permutation: &Permutation) {
        self.permutation.to_mut().permute_mut(permutation);
    }
}

impl<'a, T: Support> Support for Permuting<'a, T> {
    fn support(&self) -> HashSet<Name> {
        let mut support = self.value.support();
        support.permute_mut(&self.permutation);
        support
    }
}

impl<'a, T> Permuting<'a, T> {
    pub fn into_inner(self) -> T
    where
        T: Permute,
    {
        self.value.permute(&self.permutation)
    }

    pub fn map<U>(self, f: impl Fn(T) -> U) -> Permuting<'a, U> {
        Permuting {
            permutation: self.permutation,
            value: f(self.value),
        }
    }
}
