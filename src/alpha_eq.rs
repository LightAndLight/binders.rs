use std::borrow::Cow;

use crate::{name::Name, permuting::Permuting};

pub trait AlphaEq {
    fn alpha_eq_under(a: Permuting<&Self>, b: Permuting<&Self>) -> bool;

    fn alpha_eq(&self, other: &Self) -> bool {
        AlphaEq::alpha_eq_under(Permuting::from(self), Permuting::from(other))
    }
}

impl<A: AlphaEq, B: AlphaEq> AlphaEq for (A, B) {
    fn alpha_eq_under(a: Permuting<&(A, B)>, b: Permuting<&(A, B)>) -> bool {
        A::alpha_eq_under(
            Permuting {
                permutation: Cow::Borrowed(&a.permutation),
                value: &a.value.0,
            },
            Permuting {
                permutation: Cow::Borrowed(&b.permutation),
                value: &b.value.0,
            },
        ) && B::alpha_eq_under(
            Permuting {
                permutation: a.permutation,
                value: &a.value.1,
            },
            Permuting {
                permutation: b.permutation,
                value: &b.value.1,
            },
        )
    }
}

impl AlphaEq for Name {
    fn alpha_eq_under(a: Permuting<&Name>, b: Permuting<&Name>) -> bool {
        a.map(|name| *name).into_inner() == b.map(|name| *name).into_inner()
    }
}

impl<T: AlphaEq> AlphaEq for Box<T> {
    fn alpha_eq_under(a: Permuting<&Box<T>>, b: Permuting<&Box<T>>) -> bool {
        T::alpha_eq_under(
            Permuting {
                permutation: a.permutation,
                value: a.value.as_ref(),
            },
            Permuting {
                permutation: b.permutation,
                value: b.value.as_ref(),
            },
        )
    }
}

impl<T: AlphaEq> AlphaEq for &T {
    fn alpha_eq_under(a: Permuting<&&T>, b: Permuting<&&T>) -> bool {
        T::alpha_eq_under(
            Permuting {
                permutation: a.permutation,
                value: *a.value,
            },
            Permuting {
                permutation: b.permutation,
                value: *b.value,
            },
        )
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use super::AlphaEq;
    use crate::{name::fresh, permutation::Permutation, permuting::Permuting};

    #[test]
    fn test_1() {
        let name = fresh();
        assert!(name.alpha_eq(&name));
    }

    #[test]
    fn test_2() {
        let name1 = fresh();
        let name2 = fresh();
        let name3 = fresh();

        assert!(AlphaEq::alpha_eq_under(
            Permuting {
                permutation: Cow::Owned(Permutation::swap(name1, name3)),
                value: &name1
            },
            Permuting {
                permutation: Cow::Owned(Permutation::swap(name2, name3)),
                value: &name2
            }
        ));
    }
}

/** A wrapper that uses alpha equality ([`alpha_eq`]) for equality (`==`).

`Alpha(a) == Alpha(b)` means `a.alpha_eq(b)`.
*/
#[derive(Debug)]
pub struct Alpha<T>(pub T);

impl<T: AlphaEq> PartialEq for Alpha<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.alpha_eq(&other.0)
    }
}

impl<T: AlphaEq> Eq for Alpha<T> {}
