use std::collections::{HashMap, HashSet};

use either::Either;

use super::name::Name;

#[derive(Clone)]
pub struct Permutation {
    value: HashMap<Name, Name>,
}

impl Permutation {
    pub fn id() -> Self {
        Permutation {
            value: HashMap::new(),
        }
    }

    pub fn swap(a: Name, b: Name) -> Self {
        Permutation {
            value: HashMap::from([(a, b), (b, a)]),
        }
    }

    pub fn apply(&self, name: Name) -> Name {
        match self.value.get(&name) {
            Some(new_name) => *new_name,
            None => name,
        }
    }

    pub fn apply_mut(&self, name: &mut Name) {
        if let Some(new_name) = self.value.get(name).copied() {
            *name = new_name;
        }
    }

    pub fn after_mut(&self, other: &mut Permutation) {
        let other_keys = other
            .value
            .iter_mut()
            .map(|(key, value)| {
                self.apply_mut(value);
                key
            })
            .copied()
            .collect::<HashSet<Name>>();

        /*
        Each name that `other` doesn't explicitly map is taken
        to map to itself. For such keys, the final permutation inherits
        the mapping from `self`: if `other(x) = x` and `self(x) = y`, then
        `self(other(x)) = (self . other)(x) = y`.
        */
        other
            .value
            .extend(self.value.iter().filter_map(|(key, value)| {
                if other_keys.contains(key) {
                    None
                } else {
                    Some((key, value))
                }
            }));
    }

    pub fn after(&self, mut other: Permutation) -> Self {
        self.after_mut(&mut other);
        other
    }

    pub fn inverse(&self) -> Self {
        Permutation {
            value: self
                .value
                .iter()
                .map(|(key, value)| (*value, *key))
                .collect(),
        }
    }
}

pub trait Permute: Sized {
    fn permute_mut(&mut self, permutation: &Permutation);

    fn permute(mut self, permutation: &Permutation) -> Self {
        self.permute_mut(permutation);
        self
    }

    fn swap_mut(&mut self, a: Name, b: Name) {
        self.permute_mut(&Permutation::swap(a, b));
    }

    fn swap(mut self, a: Name, b: Name) -> Self {
        self.swap_mut(a, b);
        self
    }
}

impl Permute for Permutation {
    fn permute_mut(&mut self, permutation: &Permutation) {
        self.value.values_mut().for_each(&|name| {
            permutation.apply_mut(name);
        });
    }
}

impl Permute for Name {
    fn permute_mut(&mut self, permutation: &Permutation) {
        permutation.apply_mut(self);
    }
}

impl<A: Permute, B: Permute> Permute for (A, B) {
    fn permute_mut(&mut self, permutation: &Permutation) {
        self.0.permute_mut(permutation);
        self.1.permute_mut(permutation);
    }
}

impl<A: Permute, B: Permute> Permute for Either<A, B> {
    fn permute_mut(&mut self, permutation: &Permutation) {
        match self {
            Either::Left(left) => {
                left.permute_mut(permutation);
            }
            Either::Right(right) => {
                right.permute_mut(permutation);
            }
        }
    }
}

impl<T: Eq + std::hash::Hash + Permute> Permute for HashSet<T> {
    fn permute_mut(&mut self, permutation: &Permutation) {
        let names = std::mem::take(self);
        names.into_iter().for_each(|mut value| {
            value.permute_mut(permutation);
            self.insert(value);
        });
    }
}

impl<T: Permute> Permute for Box<T> {
    fn permute_mut(&mut self, permutation: &Permutation) {
        self.as_mut().permute_mut(permutation)
    }
}
