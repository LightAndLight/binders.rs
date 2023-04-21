use std::{borrow::Cow, collections::HashSet};

use super::{
    alpha_eq::AlphaEq,
    name::{fresh, Name},
    permutation::{Permutation, Permute},
    permuting::Permuting,
    rename::Rename,
    subst::Subst,
    support::Support,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Binder<T> {
    name: Name,
    body: T,
}

impl<T: Permute> Permute for Binder<T> {
    fn permute_mut(&mut self, permutation: &Permutation) {
        self.name.permute_mut(permutation);
        self.body.permute_mut(permutation);
    }
}

impl<T: Rename> Rename for Binder<T> {
    fn rename_mut(&mut self, f: &dyn Fn(&mut Name)) {
        let old_name = self.name;
        let new_name = fresh();

        self.name = new_name;
        self.body.rename_mut(&|name| {
            if *name == old_name {
                *name = new_name;
            } else {
                f(name);
            }
        });
    }
}

impl<V, T: Subst<V>> Subst<V> for Binder<T> {
    fn name(body: Name) -> V {
        T::name(body)
    }

    fn subst_mut(&mut self, f: &dyn Fn(Name, &mut V)) {
        let old_name = self.name;
        let new_name = fresh();

        self.name = new_name;
        self.body.subst_mut(&|name, value| {
            if name == old_name {
                *value = T::name(new_name);
            } else {
                f(name, value);
            }
        });
    }
}

impl<T: Support> Support for Binder<T> {
    fn support(&self) -> HashSet<Name> {
        let mut support = self.body.support();
        support.remove(&self.name);
        support
    }
}

impl<T: AlphaEq> AlphaEq for Binder<T> {
    fn alpha_eq_under(a: Permuting<&Binder<T>>, b: Permuting<&Binder<T>>) -> bool {
        let actual_a = a.permutation.apply(a.value.name);
        let actual_b = b.permutation.apply(b.value.name);

        if actual_a == actual_b {
            T::alpha_eq_under(
                Permuting {
                    permutation: a.permutation,
                    value: &a.value.body,
                },
                Permuting {
                    permutation: b.permutation,
                    value: &b.value.body,
                },
            )
        } else {
            let name = fresh();
            T::alpha_eq_under(
                Permuting {
                    permutation: Cow::Owned(
                        Permutation::swap(actual_a, name).after(a.permutation.into_owned()),
                    ),
                    value: &a.value.body,
                },
                Permuting {
                    permutation: Cow::Owned(
                        Permutation::swap(actual_b, name).after(b.permutation.into_owned()),
                    ),
                    value: &b.value.body,
                },
            )
        }
    }
}

impl<T> Binder<T> {
    pub fn new(f: impl FnOnce(Name) -> T) -> Binder<T> {
        let name = fresh();
        Binder {
            name,
            body: f(name),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Binder;
    use crate::nominal::alpha_eq::AlphaEq;

    #[test]
    fn test_1() {
        let binder1 = Binder::new(|name| name);
        let binder2 = Binder::new(|name| name);

        assert!(binder1.alpha_eq(&binder2));
    }
}
