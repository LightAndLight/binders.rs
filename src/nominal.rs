pub mod alpha_eq;
pub mod binder;
pub mod name;
pub mod permutation;
pub mod permuting;
pub mod subst;
pub mod support;

use std::collections::HashSet;

use binder::Binder;
use name::Name;
use permutation::Permute;
use permuting::Permuting;
use support::Support;

pub trait Nominal: Permute + Support {}

impl Nominal for Name {}
impl<T: Eq + std::hash::Hash + Nominal> Nominal for HashSet<T> {}
impl<T: Nominal> Nominal for Binder<T> {}
impl<'a, T: Nominal> Nominal for Permuting<'a, T> {}

#[cfg(test)]
mod test {
    use std::{
        collections::{HashMap, HashSet},
        io::Write,
    };

    use crate::nominal::{
        alpha_eq::AlphaEq,
        binder::Binder,
        name::Name,
        permutation::{Permutation, Permute},
        permuting::Permuting,
        subst::Subst,
        support::Support,
        Nominal,
    };

    #[derive(Debug, PartialEq, Eq)]
    enum Expr {
        Var(Name),
        Lam(String, Box<Binder<Expr>>),
        App(Box<Expr>, Box<Expr>),
    }

    impl Permute for Expr {
        fn permute_mut(&mut self, permutation: &Permutation) {
            match self {
                Expr::Var(name) => {
                    permutation.apply_mut(name);
                }
                Expr::Lam(_label, binder) => binder.permute_mut(permutation),
                Expr::App(a, b) => {
                    a.permute_mut(permutation);
                    b.permute_mut(permutation);
                }
            }
        }
    }

    impl Subst<Expr> for Expr {
        fn name(name: Name) -> Expr {
            Expr::Var(name)
        }

        fn subst_mut(&mut self, f: &dyn Fn(Name, &mut Expr)) {
            match self {
                Expr::Var(name) => {
                    f(*name, self);
                }
                Expr::Lam(_label, binder) => binder.subst_mut(f),
                Expr::App(a, b) => {
                    a.subst_mut(f);
                    b.subst_mut(f);
                }
            }
        }
    }

    impl Support for Expr {
        fn support(&self) -> HashSet<Name> {
            match self {
                Expr::Var(name) => HashSet::from([*name]),
                Expr::Lam(_label, binder) => binder.support(),
                Expr::App(a, b) => {
                    let mut support = a.support();
                    support.extend(b.support());
                    support
                }
            }
        }
    }

    impl Nominal for Expr {}

    impl AlphaEq for Expr {
        fn alpha_eq_under(a: Permuting<&Expr>, b: Permuting<&Expr>) -> bool {
            match (a.value, b.value) {
                (Expr::Var(name1), Expr::Var(name2)) => Name::alpha_eq_under(
                    Permuting {
                        permutation: a.permutation,
                        value: name1,
                    },
                    Permuting {
                        permutation: b.permutation,
                        value: name2,
                    },
                ),
                (Expr::Lam(_, binder1), Expr::Lam(_, binder2)) => Binder::alpha_eq_under(
                    Permuting {
                        permutation: a.permutation,
                        value: binder1,
                    },
                    Permuting {
                        permutation: b.permutation,
                        value: binder2,
                    },
                ),
                (Expr::App(left1, right1), Expr::App(left2, right2)) => <(_, _)>::alpha_eq_under(
                    Permuting {
                        permutation: a.permutation,
                        value: &(left1, right1),
                    },
                    Permuting {
                        permutation: b.permutation,
                        value: &(left2, right2),
                    },
                ),
                _ => false,
            }
        }
    }

    impl Expr {
        fn print(&self, names: &mut HashMap<Name, String>, buffer: &mut dyn Write) {
            match self {
                Expr::Var(name) => buffer
                    .write_all(names.get(name).unwrap().as_bytes())
                    .unwrap(),
                Expr::Lam(label, binder) => {
                    buffer.write_all("\\".as_bytes()).unwrap();
                    buffer.write_all(label.as_bytes()).unwrap();
                    buffer.write_all(" -> ".as_bytes()).unwrap();

                    binder.fold(|name, body| {
                        names.insert(name, label.clone());
                        body.print(names, buffer);
                        names.remove(&name);
                    });
                }
                Expr::App(a, b) => {
                    if matches!(a.as_ref(), Expr::Lam { .. } | Expr::App(_, _)) {
                        buffer.write_all("(".as_bytes()).unwrap();
                    }

                    a.print(names, buffer);

                    if matches!(a.as_ref(), Expr::Lam { .. } | Expr::App(_, _)) {
                        buffer.write_all(")".as_bytes()).unwrap();
                    }

                    buffer.write_all(" ".as_bytes()).unwrap();

                    b.print(names, buffer);
                }
            }
        }
    }

    #[test]
    fn test_1() {
        let expr1 = Expr::Lam(String::from("x"), Box::new(Binder::new(Expr::Var)));
        let expr2 = Expr::Lam(String::from("y"), Box::new(Binder::new(Expr::Var)));

        assert!(expr1.alpha_eq(&expr2));
    }

    #[test]
    fn test_2() {
        // \x -> \y -> x
        let expr1 = Expr::Lam(
            String::from("x"),
            Box::new(Binder::new(|x| {
                Expr::Lam(String::from("y"), Box::new(Binder::new(|_y| Expr::Var(x))))
            })),
        );

        // \x -> \y -> y
        let expr2 = Expr::Lam(
            String::from("x"),
            Box::new(Binder::new(|_x| {
                #[allow(clippy::redundant_closure)]
                Expr::Lam(String::from("y"), Box::new(Binder::new(|y| Expr::Var(y))))
            })),
        );

        assert!(!expr1.alpha_eq(&expr2));
    }

    #[test]
    fn test_3() {
        // \x -> \y -> x
        let expr1 = Expr::Lam(
            String::from("x"),
            Box::new(Binder::new(|x| {
                Expr::Lam(String::from("y"), Box::new(Binder::new(|_y| Expr::Var(x))))
            })),
        );

        // \y -> \x -> y
        let expr2 = Expr::Lam(
            String::from("y"),
            Box::new(Binder::new(|y| {
                Expr::Lam(String::from("x"), Box::new(Binder::new(|_x| Expr::Var(y))))
            })),
        );

        assert!(expr1.alpha_eq(&expr2));
    }

    #[test]
    fn print_1() {
        // \x -> \y -> x
        let expr = Expr::Lam(
            String::from("x"),
            Box::new(Binder::new(|x| {
                Expr::Lam(String::from("y"), Box::new(Binder::new(|_y| Expr::Var(x))))
            })),
        );

        let mut names = HashMap::new();
        let mut buffer = Vec::new();
        expr.print(&mut names, &mut buffer);
        assert_eq!(buffer, "\\x -> \\y -> x".as_bytes())
    }
}
