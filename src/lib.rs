/** A de Bruijn index.

Denoted by `#0`, `#1`, `#2`, etc.
*/
#[derive(Debug)]
pub struct Index {
    value: usize,
}

impl Index {
    fn value(&self) -> usize {
        self.value
    }
}

pub trait Context {
    type Item;
    fn new() -> Self;
    fn len(&self) -> usize;
    fn push(&mut self, value: Self::Item);
    fn pop(&mut self);
    fn get_level<'a>(&'a self, level: &Level) -> &'a Self::Item;
}

pub trait ContextExt: Context {
    fn get_index<'a>(&'a self, var: &Index) -> &'a Self::Item;
}

impl<C: Context> ContextExt for C {
    /** When new context entries are added to the end of a context, then de Bruijn indices
    can be seen as indexing the context from the end. When this is the case, it's very naturally
    implemented in terms of de Bruijn levels.
    */
    fn get_index<'a>(&'a self, var: &Index) -> &'a Self::Item {
        let level = Depth { value: self.len() }.index_to_level(var);

        self.get_level(&level)
    }
}

mod context {
    use super::{Context, Level};

    /** A context.

    Denoted by `[a, b, ..., z]`.
    */
    pub struct Sequence<A> {
        value: Vec<A>,
    }

    impl<A> Context for Sequence<A> {
        type Item = A;

        fn new() -> Self {
            Self { value: vec![] }
        }

        fn len(&self) -> usize {
            self.value.len()
        }

        fn push(&mut self, value: A) {
            self.value.push(value)
        }

        fn pop(&mut self) {
            self.value.pop();
        }

        /** When new context entries are added to the end of a context, then de Bruijn level
        can be seen as indexing the context from the start.
        */
        fn get_level(&self, level: &Level) -> &A {
            &self.value[level.value - 1]
        }
    }

    pub struct Ignore {
        len: usize,
    }

    impl Context for Ignore {
        type Item = ();

        fn new() -> Self {
            Self { len: 0 }
        }

        fn len(&self) -> usize {
            self.len
        }

        fn push(&mut self, (): ()) {
            self.len += 1;
        }

        fn pop(&mut self) {
            self.len -= 1;
        }

        fn get_level<'a>(&'a self, _level: &Level) -> &'a Self::Item {
            &()
        }
    }
}

/** A binding form.

Allows the inner type `T` to refer to an additional variable.
*/
#[derive(Debug)]
pub struct Binder<T> {
    body: T,
}

impl<'a, T, U> Binder<(&'a T, &'a U)> {
    pub fn pair(left: &'a Binder<T>, right: &'a Binder<U>) -> Self {
        Binder {
            body: (&left.body, &right.body),
        }
    }
}

impl<T: Eq> PartialEq for Binder<T> {
    /** Equality assumes that any [`Index`]s in the left and right hand sides
    index the same context. We pretend to judge them both against a single
    "open" context:

    ```ignore
    let mut context = Open::new();
    Binder::pair(self, other).elim(&mut context, (), |context, (t1, t2)| t1 == t2)
    ```
    */
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body
    }
}

impl<T: Eq> Eq for Binder<T> {}

/** A de Bruijn level.

A de Bruijn level can be thought of a de Bruijn index "placed in context". This is denoted by `[_, _, ..., _]#n`,
where `n` is a de Bruijn index.
*/
#[derive(Debug, Clone, Copy)]
pub struct Level {
    value: usize,
}

impl Level {
    pub fn value(&self) -> usize {
        self.value
    }

    /** De Bruijn indices, insofar as they represent variables, aren't comparable in isolation.
    Syntactically, indices `#0` and `#1` might be different, but *semantically* they can represent the
    same variable under different contexts. For example, `get([a], #0)` and `get([a, b], #1)` will return
    the same result because they're the same variable.

    [`Level`]s take into account the context, so equality of [`Level`]s is also equality of variables.
    */
    fn eq(&self, other: &Level) -> bool {
        self.value == other.value
    }
}

impl PartialEq for Level {
    fn eq(&self, other: &Self) -> bool {
        Level::eq(self, other)
    }
}

impl Eq for Level {}

/// The abstract notion of a variable.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Var {
    value: Level,
}

impl Var {
    pub fn as_level<C: Context>(&self, _context: &C) -> Level {
        Level {
            value: self.value.value,
        }
    }

    pub fn as_index<C: Context>(&self, context: &C) -> Index {
        Depth {
            value: context.len(),
        }
        .level_to_index(&self.value)
    }
}

pub struct Depth {
    value: usize,
}

/**
At any specific binding depth, [`Index`]s and [`Level`]s are isomorphic (as long as the
[`Level`]s do not exceed the depth).
*/
impl Depth {
    pub fn index_to_level(&self, var: &Index) -> Level {
        Level {
            value: self.value - var.value,
        }
    }

    pub fn level_to_index(&self, level: &Level) -> Index {
        Index {
            value: self.value - level.value,
        }
    }
}

pub struct Scope {
    depth: Depth,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            depth: Depth { value: 0 },
        }
    }

    /**
    [`Binder`] introduction.

    ```rust
    use debruijn::Scope;

    let mut scope = Scope::new();
    let _ = scope.binder(|scope, x| scope.binder(|scope, y| (x, y)));
    ```
    */
    pub fn binder<T, F: Fn(&mut Scope, Var) -> T>(&mut self, f: F) -> Binder<T> {
        self.depth.value += 1;

        let var = Var {
            value: Level {
                value: self.depth.value,
            },
        };
        let body = f(self, var);

        self.depth.value -= 1;

        Binder { body }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Binder<T> {
    pub fn elim<A, R, C: Context<Item = A>, F: Fn(&mut C, T) -> R>(
        self,
        context: &mut C,
        value: A,
        f: F,
    ) -> R {
        context.push(value);
        let result = f(context, self.body);
        context.pop();
        result
    }

    pub fn elim_ref<A, R, C: Context<Item = A>, F: FnOnce(&mut C, &T) -> R>(
        &self,
        context: &mut C,
        value: A,
        f: F,
    ) -> R {
        context.push(value);
        let result = f(context, &self.body);
        context.pop();
        result
    }
}

pub trait Subst<Value> {
    fn subst(self, target: Var, replacement: Value) -> Self;
    fn subst_mut(&mut self, target: Var, replacement: Value);
}

impl<Value, T: Subst<Value>> Subst<Value> for Binder<T> {
    fn subst(self, target: Var, replacement: Value) -> Self {
        todo!()
    }

    fn subst_mut(&mut self, target: Var, replacement: Value) {
        todo!()
    }
}

/*
pub trait Subst<Name, Value> {
    fn subst(self, target: Name, replacement: Value) -> Self;
    fn subst_mut(&mut self, target: Name, replacement: Value);
}

mod system_f {
    use super::Subst;

    struct ExprName;
    struct Expr;

    struct TypeName;
    struct Type;

    impl Subst<ExprName, Expr> for Expr {
        fn subst(self, target: ExprName, replacement: Expr) -> Self {
            todo!()
        }

        fn subst_mut(&mut self, target: ExprName, replacement: Expr) {
            todo!()
        }
    }

    impl Subst<TypeName, Type> for Expr {
        fn subst(self, target: TypeName, replacement: Type) -> Self {
            todo!()
        }

        fn subst_mut(&mut self, target: TypeName, replacement: Type) {
            todo!()
        }
    }

    impl Subst<TypeName, Type> for Type {
        fn subst(self, target: TypeName, replacement: Type) -> Self {
            todo!()
        }

        fn subst_mut(&mut self, target: TypeName, replacement: Type) {
            todo!()
        }
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    enum Expr {
        Var(Var),
        Lam(String, Box<Binder<Expr>>),
        App(Box<Expr>, Box<Expr>),
    }

    fn print_dbi(buffer: &mut String, context: &mut context::Ignore, expr: &Expr) {
        use std::fmt::Write;

        match expr {
            Expr::Var(var) => {
                buffer.push('#');
                write!(buffer, "{:?}", var.as_index(context).value()).unwrap();
            }
            Expr::Lam(_name, scope) => scope.elim_ref(context, (), |context, body| {
                buffer.push_str("lam(");
                print_dbi(buffer, context, body);
                buffer.push(')');
            }),
            Expr::App(a, b) => {
                buffer.push_str("app(");
                print_dbi(buffer, context, a);
                buffer.push(',');
                print_dbi(buffer, context, b);
                buffer.push(')');
            }
        }
    }

    #[test]
    fn print_dbi_1() {
        let mut scope = Scope::new();
        let expr = Expr::Lam(
            String::new(),
            Box::new(scope.binder(|scope, x| {
                Expr::Lam(
                    String::new(),
                    Box::new(scope.binder(|_scope, y| {
                        Expr::App(Box::new(Expr::Var(x)), Box::new(Expr::Var(y)))
                    })),
                )
            })),
        );

        let mut buffer = String::new();
        let mut context = Context::new();
        print_dbi(&mut buffer, &mut context, &expr);
        assert_eq!(buffer, "lam(lam(app(#1,#0)))")
    }

    #[test]
    fn print_dbi_2() {
        let mut scope = Scope::new();
        let expr = Expr::Lam(
            String::new(),
            Box::new(scope.binder(|scope, x| {
                Expr::App(
                    Box::new(Expr::Var(x)),
                    Box::new(Expr::Lam(
                        String::new(),
                        Box::new(scope.binder(|_scope, y| {
                            Expr::App(Box::new(Expr::Var(x)), Box::new(Expr::Var(y)))
                        })),
                    )),
                )
            })),
        );

        let mut buffer = String::new();
        let mut context = Context::new();
        print_dbi(&mut buffer, &mut context, &expr);
        assert_eq!(buffer, "lam(app(#0,lam(app(#1,#0))))")
    }

    fn print_named(buffer: &mut String, context: &mut context::Sequence<String>, expr: &Expr) {
        match expr {
            Expr::Var(var) => {
                buffer.push_str(context.get_level(&var.as_level(context)));
            }
            Expr::Lam(name, scope) => scope.elim_ref(context, name.clone(), |context, body| {
                buffer.push_str("lam(");
                buffer.push_str(name);
                buffer.push(',');
                print_named(buffer, context, body);
                buffer.push(')');
            }),
            Expr::App(a, b) => {
                buffer.push_str("app(");
                print_named(buffer, context, a);
                buffer.push(',');
                print_named(buffer, context, b);
                buffer.push(')');
            }
        }
    }

    #[test]
    fn print_named_1() {
        let mut scope = Scope::new();
        let expr = Expr::Lam(
            String::from("x"),
            Box::new(scope.binder(|scope, x| {
                Expr::Lam(
                    String::from("y"),
                    Box::new(scope.binder(|_scope, y| {
                        Expr::App(Box::new(Expr::Var(x)), Box::new(Expr::Var(y)))
                    })),
                )
            })),
        );

        let mut buffer = String::new();
        let mut context = Context::new();
        print_named(&mut buffer, &mut context, &expr);
        assert_eq!(buffer, "lam(x,lam(y,app(x,y)))")
    }

    #[test]
    fn print_named_2() {
        let mut scope = Scope::new();
        let expr = Expr::Lam(
            String::from("x"),
            Box::new(scope.binder(|scope, x| {
                Expr::App(
                    Box::new(Expr::Var(x)),
                    Box::new(Expr::Lam(
                        String::from("y"),
                        Box::new(scope.binder(|_scope, y| {
                            Expr::App(Box::new(Expr::Var(x)), Box::new(Expr::Var(y)))
                        })),
                    )),
                )
            })),
        );

        let mut buffer = String::new();
        let mut context = Context::new();
        print_named(&mut buffer, &mut context, &expr);
        assert_eq!(buffer, "lam(x,app(x,lam(y,app(x,y))))")
    }
}
