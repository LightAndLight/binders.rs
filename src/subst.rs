use super::name::Name;
use super::permutation::Permutable;

pub trait Subst<T>: Permutable {
    fn name(name: Name) -> T;

    fn subst_mut(&mut self, f: &dyn Fn(Name, &mut T));

    fn subst(mut self, f: &dyn Fn(Name) -> T) -> Self {
        self.subst_mut(&|name, term| {
            *term = f(name);
        });
        self
    }
}
