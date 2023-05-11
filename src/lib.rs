pub mod alpha_eq;
pub mod binder;
pub mod context;
pub mod name;
pub mod permutation;
pub mod permuting;
pub mod subst;
pub mod support;

use std::collections::HashSet;

use binder::Binder;
use name::Name;
use permutation::Permutable;
use permuting::Permuting;
use support::Supported;

pub trait Nominal: Permutable + Supported {}

impl Nominal for Name {}
impl<T: Eq + std::hash::Hash + Nominal> Nominal for HashSet<T> {}
impl<T: Nominal> Nominal for Binder<T> {}
impl<'a, T: Nominal> Nominal for Permuting<'a, T> {}

#[cfg(test)]
mod test {
    mod lambda_calculus {
        use std::{
            collections::{HashMap, HashSet},
            io::Write,
        };

        use crate::{
            alpha_eq::AlphaEq,
            binder::Binder,
            name::Name,
            permutation::{Permutable, Permutation},
            permuting::Permuting,
            subst::Subst,
            support::Supported,
            Nominal,
        };

        #[derive(Debug, PartialEq, Eq)]
        enum Expr {
            Var(Name),
            Lam(String, Box<Binder<Expr>>),
            App(Box<Expr>, Box<Expr>),
        }

        impl Permutable for Expr {
            fn permute_by_mut(&mut self, permutation: &Permutation) {
                match self {
                    Expr::Var(name) => {
                        permutation.apply_mut(name);
                    }
                    Expr::Lam(_label, binder) => binder.permute_by_mut(permutation),
                    Expr::App(a, b) => {
                        a.permute_by_mut(permutation);
                        b.permute_by_mut(permutation);
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

        impl Supported for Expr {
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
                    (Expr::App(left1, right1), Expr::App(left2, right2)) => {
                        <(_, _)>::alpha_eq_under(
                            Permuting {
                                permutation: a.permutation,
                                value: &(left1, right1),
                            },
                            Permuting {
                                permutation: b.permutation,
                                value: &(left2, right2),
                            },
                        )
                    }
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

                        binder.unbind_ref(|name, body| {
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
            let expr1 = Expr::Lam(String::from("x"), Box::new(Binder::bind(Expr::Var)));
            let expr2 = Expr::Lam(String::from("y"), Box::new(Binder::bind(Expr::Var)));

            assert!(expr1.alpha_eq(&expr2));
        }

        #[test]
        fn test_2() {
            // \x -> \y -> x
            let expr1 = Expr::Lam(
                String::from("x"),
                Box::new(Binder::bind(|x| {
                    Expr::Lam(String::from("y"), Box::new(Binder::bind(|_y| Expr::Var(x))))
                })),
            );

            // \x -> \y -> y
            let expr2 = Expr::Lam(
                String::from("x"),
                Box::new(Binder::bind(|_x| {
                    #[allow(clippy::redundant_closure)]
                    Expr::Lam(String::from("y"), Box::new(Binder::bind(|y| Expr::Var(y))))
                })),
            );

            assert!(!expr1.alpha_eq(&expr2));
        }

        #[test]
        fn test_3() {
            // \x -> \y -> x
            let expr1 = Expr::Lam(
                String::from("x"),
                Box::new(Binder::bind(|x| {
                    Expr::Lam(String::from("y"), Box::new(Binder::bind(|_y| Expr::Var(x))))
                })),
            );

            // \y -> \x -> y
            let expr2 = Expr::Lam(
                String::from("y"),
                Box::new(Binder::bind(|y| {
                    Expr::Lam(String::from("x"), Box::new(Binder::bind(|_x| Expr::Var(y))))
                })),
            );

            assert!(expr1.alpha_eq(&expr2));
        }

        #[test]
        fn print_1() {
            // \x -> \y -> x
            let expr = Expr::Lam(
                String::from("x"),
                Box::new(Binder::bind(|x| {
                    Expr::Lam(String::from("y"), Box::new(Binder::bind(|_y| Expr::Var(x))))
                })),
            );

            let mut names = HashMap::new();
            let mut buffer = Vec::new();
            expr.print(&mut names, &mut buffer);
            assert_eq!(buffer, "\\x -> \\y -> x".as_bytes())
        }
    }

    mod system_f {
        use std::collections::HashMap;

        use crate::{
            binder::Binder,
            context::Context,
            name::Name,
            permutation::{Permutable, Permutation},
            subst::Subst,
        };

        #[derive(Debug, PartialEq, Eq, Clone)]
        enum Kind {
            Type,
            Arrow(Box<Kind>, Box<Kind>),
        }

        #[derive(Debug, PartialEq, Eq, Clone)]
        enum Type {
            Var(Name),
            Forall(String, Kind, Box<Binder<Type>>),
            Arrow,
            App(Box<Type>, Box<Type>),
        }

        impl Type {
            fn match_arrow(&self) -> Option<(&Type, &Type)> {
                match self {
                    Type::App(a, out_ty) => match a.as_ref() {
                        Type::App(b, in_ty) => match b.as_ref() {
                            Type::Arrow => Some((in_ty, out_ty)),
                            _ => None,
                        },
                        _ => None,
                    },
                    _ => None,
                }
            }
        }

        impl Permutable for Type {
            fn permute_by_mut(&mut self, permutation: &Permutation) {
                match self {
                    Type::Var(name) => {
                        name.permute_by_mut(permutation);
                    }
                    Type::Forall(_name, _kind, binder) => {
                        binder.permute_by_mut(permutation);
                    }
                    Type::Arrow => {}
                    Type::App(a, b) => {
                        a.permute_by_mut(permutation);
                        b.permute_by_mut(permutation);
                    }
                }
            }
        }

        impl Subst<Type> for Type {
            fn name(name: Name) -> Type {
                Type::Var(name)
            }

            fn subst_mut(&mut self, f: &dyn Fn(Name, &mut Type)) {
                match self {
                    Type::Var(name) => {
                        f(*name, self);
                    }
                    Type::Forall(_name, _kind, binder) => {
                        binder.subst_mut(f);
                    }
                    Type::Arrow => {}
                    Type::App(a, b) => {
                        a.subst_mut(f);
                        b.subst_mut(f);
                    }
                }
            }
        }

        #[derive(Debug, PartialEq, Eq, Clone)]
        enum Expr {
            Var(Name),
            Lam(String, Type, Box<Binder<Expr>>),
            App(Box<Expr>, Box<Expr>),
            TyLam(String, Kind, Box<Binder<Expr>>),
            TyApp(Box<Expr>, Type),
        }

        impl Permutable for Expr {
            fn permute_by_mut(&mut self, permutation: &Permutation) {
                match self {
                    Expr::Var(name) => {
                        name.permute_by_mut(permutation);
                    }
                    Expr::Lam(_name, _ty, binder) => binder.permute_by_mut(permutation),
                    Expr::App(a, b) => {
                        a.permute_by_mut(permutation);
                        b.permute_by_mut(permutation);
                    }
                    Expr::TyLam(_name, _kind, binder) => {
                        binder.permute_by_mut(permutation);
                    }
                    Expr::TyApp(expr, _ty) => {
                        expr.permute_by_mut(permutation);
                    }
                }
            }
        }

        impl Subst<Type> for Expr {
            fn name(name: Name) -> Type {
                Type::Var(name)
            }

            fn subst_mut(&mut self, f: &dyn Fn(Name, &mut Type)) {
                match self {
                    Expr::Var(_) => {}
                    Expr::Lam(_name, ty, binder) => {
                        ty.subst_mut(f);
                        binder.subst_mut(f);
                    }
                    Expr::App(a, b) => {
                        a.subst_mut(f);
                        b.subst_mut(f);
                    }
                    Expr::TyLam(_name, _kind, binder) => {
                        binder.subst_mut(f);
                    }
                    Expr::TyApp(expr, ty) => {
                        expr.subst_mut(f);
                        ty.subst_mut(f);
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
                    Expr::Lam(_name, _ty, binder) => binder.subst_mut(f),
                    Expr::App(a, b) => {
                        a.subst_mut(f);
                        b.subst_mut(f);
                    }
                    Expr::TyLam(_name, _kind, binder) => {
                        binder.subst_mut(f);
                    }
                    Expr::TyApp(expr, _ty) => {
                        expr.subst_mut(f);
                    }
                }
            }
        }

        enum InferError {
            VarNotInScope { variable: String },
            TypeVarNotInScope { variable: String },
            KindMismatch { expected_kind: Kind, got_kind: Kind },
            ExpectedArrowKind { got_kind: Kind },
            TypeMismatch { expected_type: Type, got_type: Type },
            ExpectedArrowType { got_type: Type },
            ExpectedForallType { got_type: Type },
        }

        fn infer_kind(
            variables: &mut impl Context<Item = String>,
            kinds: &mut impl Context<Item = Kind>,
            ty: &Type,
        ) -> Result<Kind, InferError> {
            match ty {
                Type::Var(name) => match kinds.get(name) {
                    Some(kind) => Ok(kind.clone()),
                    None => Err(InferError::TypeVarNotInScope {
                        variable: variables.get(name).unwrap().clone(),
                    }),
                },
                Type::Forall(var, kind, binder) => binder.unbind_ref(|name, body| {
                    variables.with(name, var.clone(), |variables| {
                        kinds.with(name, kind.clone(), |kinds| {
                            let result = infer_kind(variables, kinds, body)?;
                            match result {
                                Kind::Type => Ok(Kind::Type),
                                _ => Err(InferError::KindMismatch {
                                    expected_kind: Kind::Type,
                                    got_kind: result,
                                }),
                            }
                        })
                    })
                }),
                Type::Arrow => Ok(Kind::Arrow(Box::new(Kind::Type), Box::new(Kind::Type))),
                Type::App(a, b) => {
                    let a_kind = infer_kind(variables, kinds, a)?;
                    let (in_kind, out_kind) = match a_kind {
                        Kind::Arrow(in_kind, out_kind) => Ok((in_kind, out_kind)),
                        _ => Err(InferError::ExpectedArrowKind { got_kind: a_kind }),
                    }?;

                    let b_kind = infer_kind(variables, kinds, b)?;
                    if in_kind.as_ref() == &b_kind {
                        Ok(*out_kind)
                    } else {
                        Err(InferError::KindMismatch {
                            expected_kind: *in_kind,
                            got_kind: b_kind,
                        })
                    }
                }
            }
        }

        fn infer_type(
            variables: &mut HashMap<Name, String>,
            kinds: &mut HashMap<Name, Kind>,
            types: &mut HashMap<Name, Type>,
            expr: &Expr,
        ) -> Result<Type, InferError> {
            match expr {
                Expr::Var(name) => match types.get(name) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(InferError::VarNotInScope {
                        variable: variables.get(name).unwrap().clone(),
                    }),
                },
                Expr::Lam(var, in_ty, binder) => binder.unbind_ref(|name, body| {
                    variables.with(name, var.clone(), |variables| {
                        types.with(name, in_ty.clone(), |types| {
                            let out_ty = infer_type(variables, kinds, types, body)?;

                            Ok(Type::App(
                                Box::new(Type::App(Box::new(Type::Arrow), Box::new(in_ty.clone()))),
                                Box::new(out_ty),
                            ))
                        })
                    })
                }),
                Expr::App(a, b) => {
                    let a_ty = infer_type(variables, kinds, types, a)?;
                    let b_ty = infer_type(variables, kinds, types, b)?;

                    let (in_ty, out_ty) = match a_ty.match_arrow() {
                        Some(result) => Ok(result),
                        None => Err(InferError::ExpectedArrowType {
                            got_type: a_ty.clone(),
                        }),
                    }?;

                    if &b_ty == in_ty {
                        Ok(out_ty.clone())
                    } else {
                        Err(InferError::TypeMismatch {
                            expected_type: in_ty.clone(),
                            got_type: b_ty,
                        })
                    }
                }
                Expr::TyLam(var, kind, binder) => binder.unbind_ref(|name, body| {
                    variables.with(name, var.clone(), |variables| {
                        kinds.with(name, kind.clone(), |kinds| {
                            let ty = infer_type(variables, kinds, types, body)?;
                            Ok(Type::Forall(
                                var.clone(),
                                kind.clone(),
                                Box::new(Binder::bind(|ty_name| {
                                    ty.permute_by(&Permutation::swap(name, ty_name))
                                })),
                            ))
                        })
                    })
                }),
                Expr::TyApp(expr, ty) => {
                    let expr_ty = infer_type(variables, kinds, types, expr)?;
                    let (_var, kind, binder) = match expr_ty {
                        Type::Forall(var, kind, binder) => Ok((var, kind, binder)),
                        _ => Err(InferError::ExpectedForallType { got_type: expr_ty }),
                    }?;

                    let ty_kind = infer_kind(variables, kinds, ty)?;

                    if kind == ty_kind {
                        binder.unbind_ref(|name, body| {
                            Ok(body.clone().subst(&|a_name| {
                                if a_name == name {
                                    ty.clone()
                                } else {
                                    Type::name(a_name)
                                }
                            }))
                        })
                    } else {
                        Err(InferError::KindMismatch {
                            expected_kind: kind,
                            got_kind: ty_kind,
                        })
                    }
                }
            }
        }
    }
}
