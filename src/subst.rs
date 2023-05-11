use super::name::Name;
use super::permutation::Permutable;

pub trait Subst<V>: Permutable {
    fn name(name: Name) -> V;

    fn subst_mut(&mut self, f: &dyn Fn(Name, &mut V));

    fn subst(mut self, f: &dyn Fn(Name) -> V) -> Self {
        self.subst_mut(&|name, term| {
            *term = f(name);
        });
        self
    }
}

impl<V, T: Subst<V>> Subst<V> for Box<T> {
    fn name(name: Name) -> V {
        T::name(name)
    }

    fn subst_mut(&mut self, f: &dyn Fn(Name, &mut V)) {
        self.as_mut().subst_mut(f)
    }
}
