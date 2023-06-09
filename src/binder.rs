use std::{borrow::Cow, collections::HashSet};

use super::{
    alpha_eq::AlphaEq,
    name::{fresh, Name},
    permutation::{Permutable, Permutation},
    permuting::Permuting,
    subst::Subst,
    support::Supported,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Binder<T> {
    name: Name,
    body: T,
}

impl<T: Clone + Permutable> Clone for Binder<T> {
    fn clone(&self) -> Self {
        self.unbind_ref(|name, body| {
            Binder::bind(|new_name| {
                let mut body = body.clone();
                body.permute_by_mut(&Permutation::swap(name, new_name));
                body
            })
        })
    }
}

impl<T: Permutable> Permutable for Binder<T> {
    fn permute_by_mut(&mut self, permutation: &Permutation) {
        self.name.permute_by_mut(permutation);
        self.body.permute_by_mut(permutation);
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

impl<T: Supported> Supported for Binder<T> {
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
                        a.permutation
                            .into_owned()
                            .permute_by(&Permutation::swap(actual_a, name)),
                    ),
                    value: &a.value.body,
                },
                Permuting {
                    permutation: Cow::Owned(
                        b.permutation
                            .into_owned()
                            .permute_by(&Permutation::swap(actual_b, name)),
                    ),
                    value: &b.value.body,
                },
            )
        }
    }
}

impl<T> Binder<T> {
    pub fn bind(f: impl FnOnce(Name) -> T) -> Binder<T> {
        let name = fresh();
        Binder {
            name,
            body: f(name),
        }
    }

    pub fn unbind<R, F: FnOnce(Name, T) -> R>(self, f: F) -> R {
        f(self.name, self.body)
    }

    pub fn unbind_ref<R, F: FnOnce(Name, &T) -> R>(&self, f: F) -> R {
        f(self.name, &self.body)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Binder<U> {
        Binder {
            name: self.name,
            body: f(self.body),
        }
    }

    pub fn map_mut(&mut self, f: impl FnOnce(&mut T)) {
        f(&mut self.body)
    }
}

#[cfg(test)]
mod test {
    use super::Binder;
    use crate::alpha_eq::AlphaEq;

    #[test]
    fn test_1() {
        let binder1 = Binder::bind(|name| name);
        let binder2 = Binder::bind(|name| name);

        assert!(
            binder1.alpha_eq(&binder2),
            "{:?} not alpha equivalent to {:?}",
            binder1,
            binder2
        );
    }
}
