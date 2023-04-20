use std::sync::atomic::AtomicU64;

use lazy_static::lazy_static;

lazy_static! {
    static ref COUNTER: AtomicU64 = AtomicU64::new(0);
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Atom {
    value: u64,
}

fn fresh() -> Atom {
    Atom {
        value: COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
    }
}

mod name {
    use super::Atom;

    #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
    pub enum Name<N> {
        Name(N),
        Atom(Atom),
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Name<N> {
    value: name::Name<N>,
}

impl<N> Name<N> {
    fn atom(value: Atom) -> Self {
        Name {
            value: name::Name::Atom(value),
        }
    }
}

impl<N> From<N> for Name<N> {
    fn from(value: N) -> Self {
        Name {
            value: name::Name::Name(value),
        }
    }
}

pub trait Subst<V> {
    fn subst<N: Eq>(self, name: &Name<N>, value: V) -> Self;
    fn subst_mut<N: Eq>(&mut self, name: &Name<N>, value: V);
}

pub trait Swap<N>: Sized {
    fn swap_mut(&mut self, a: Name<N>, b: Name<N>);

    fn swap(mut self, a: Name<N>, b: Name<N>) -> Self {
        Self::swap_mut(&mut self, a, b);
        self
    }
}

pub trait SwapRef<N> {
    fn swap_ref(&self, a: Name<N>, b: Name<N>) -> Self;
}

impl<N: Eq> Swap<N> for Name<N> {
    fn swap_mut(&mut self, a: Name<N>, b: Name<N>) {
        if *self == a {
            *self = b;
        } else if *self == b {
            *self = a;
        }
    }
}

impl<N: Eq + Clone> SwapRef<N> for Name<N> {
    fn swap_ref(&self, a: Name<N>, b: Name<N>) -> Self {
        if self == &a {
            b
        } else if self == &b {
            a
        } else {
            self.clone()
        }
    }
}

impl<N: Clone, A: Swap<N>, B: Swap<N>> Swap<N> for (A, B) {
    fn swap_mut(&mut self, a: Name<N>, b: Name<N>) {
        self.0.swap_mut(a.clone(), b.clone());
        self.1.swap_mut(a, b);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Binder<N, T> {
    name: N,
    atom: Atom,
    body: T,
}

pub fn abs<N: Clone, T: Swap<N>>(name: N, body: T) -> Binder<N, T> {
    let atom = fresh();
    let body = body.swap(Name::from(name.clone()), Name::atom(atom));
    Binder { name, atom, body }
}

pub fn bind<N, T, F: FnOnce(Name<N>) -> T>(name: N, f: F) -> Binder<N, T> {
    let atom = fresh();
    let body = f(Name::atom(atom));
    Binder { name, atom, body }
}

impl<N, T> Binder<N, T> {
    pub fn unbind(self) -> (N, T)
    where
        N: Clone,
        T: Swap<N>,
    {
        (
            self.name.clone(),
            self.body.swap(Name::from(self.name), Name::atom(self.atom)),
        )
    }

    pub fn fold<R, F: Fn(Name<N>, T) -> R>(self, f: F) -> R {
        f(Name::atom(self.atom), self.body)
    }
}

impl<N: Eq, T: Swap<N>> Swap<N> for Binder<N, T> {
    fn swap_mut(&mut self, a: Name<N>, b: Name<N>) {
        if Name::atom(self.atom) != a && Name::atom(self.atom) != b {
            self.body.swap_mut(a, b);
        }
    }
}

impl<N: Eq + Clone, T: SwapRef<N> + Clone> SwapRef<N> for Binder<N, T> {
    fn swap_ref(&self, a: Name<N>, b: Name<N>) -> Self {
        if Name::atom(self.atom) != a && Name::atom(self.atom) != b {
            Binder {
                name: self.name.clone(),
                atom: self.atom,
                body: self.body.swap_ref(a, b),
            }
        } else {
            self.clone()
        }
    }
}

#[derive(PartialEq, Eq)]
struct Void;

pub fn pair<N1, N2, N3, T1: Swap<N1>, T2: Swap<N2>, F: FnOnce(N1, N2) -> N3>(
    combine_names: F,
    binder1: Binder<N1, T1>,
    binder2: Binder<N2, T2>,
) -> Binder<N3, (T1, T2)> {
    let atom = fresh();

    let body1 = binder1
        .body
        .swap(Name::atom(binder1.atom), Name::atom(atom));

    let body2 = binder2
        .body
        .swap(Name::atom(binder2.atom), Name::atom(atom));

    Binder {
        name: combine_names(binder1.name, binder2.name),
        atom,
        body: (body1, body2),
    }
}

pub fn pair_ref<N1, N2, N3, T1: SwapRef<N1>, T2: SwapRef<N2>, F: FnOnce(&N1, &N2) -> N3>(
    combine_names: F,
    binder1: &Binder<N1, T1>,
    binder2: &Binder<N2, T2>,
) -> Binder<N3, (T1, T2)> {
    let atom = fresh();

    let body1 = binder1
        .body
        .swap_ref(Name::atom(binder1.atom), Name::atom(atom));
    let body2 = binder2
        .body
        .swap_ref(Name::atom(binder2.atom), Name::atom(atom));

    Binder {
        name: combine_names(&binder1.name, &binder2.name),
        atom,
        body: (body1, body2),
    }
}

impl<N, T: Eq + SwapRef<N>> PartialEq for Binder<N, T> {
    fn eq(&self, other: &Self) -> bool {
        pair_ref(|_n1, _n2| (), self, other).fold(|_name, (body1, body2)| body1 == body2)
    }
}

impl<N, T: Eq + SwapRef<N>> Eq for Binder<N, T> {}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Clone)]
    enum Expr {
        Var(Name<String>),
        Lam(Box<Binder<String, Expr>>),
        App(Box<Expr>, Box<Expr>),
    }

    impl Swap<String> for Expr {
        fn swap_mut(&mut self, a: Name<String>, b: Name<String>) {
            match self {
                Expr::Var(name) => {
                    name.swap_mut(a, b);
                }
                Expr::Lam(binder) => {
                    binder.swap_mut(a, b);
                }
                Expr::App(left, right) => {
                    left.swap_mut(a.clone(), b.clone());
                    right.swap_mut(a, b);
                }
            }
        }
    }

    impl SwapRef<String> for Expr {
        fn swap_ref(&self, a: Name<String>, b: Name<String>) -> Self {
            match self {
                Expr::Var(name) => Expr::Var(name.swap_ref(a, b)),
                Expr::Lam(binder) => Expr::Lam(Box::new(binder.swap_ref(a, b))),
                Expr::App(left, right) => Expr::App(
                    Box::new(left.swap_ref(a.clone(), b.clone())),
                    Box::new(right.swap_ref(a, b)),
                ),
            }
        }
    }

    #[test]
    fn test_1() {
        let expr1 = Expr::Lam(Box::new(bind(String::from("x"), Expr::Var)));
        let expr2 = Expr::Lam(Box::new(bind(String::from("y"), Expr::Var)));
        assert_eq!(expr1, expr2);
    }

    #[test]
    fn test_2() {
        let expr1 = Expr::Lam(Box::new(bind(String::from("x"), |x| {
            Expr::Lam(Box::new(bind(String::from("y"), |_y| Expr::Var(x))))
        })));

        #[allow(clippy::redundant_closure)]
        let expr2 = Expr::Lam(Box::new(bind(String::from("x"), |_x| {
            Expr::Lam(Box::new(bind(String::from("y"), |y| Expr::Var(y))))
        })));

        assert_ne!(expr1, expr2)
    }

    #[test]
    fn test_3() {
        let expr1 = Expr::Lam(Box::new(bind(String::from("x"), |x| {
            Expr::Lam(Box::new(bind(String::from("y"), |_y| Expr::Var(x))))
        })));

        #[allow(clippy::redundant_closure)]
        let expr2 = Expr::Lam(Box::new(bind(String::from("y"), |y| {
            Expr::Lam(Box::new(bind(String::from("x"), |_x| Expr::Var(y))))
        })));

        assert_eq!(expr1, expr2)
    }
}
