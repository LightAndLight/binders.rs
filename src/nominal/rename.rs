use super::name::Name;

pub trait Rename: Sized {
    fn rename_mut(&mut self, f: &dyn Fn(&mut Name));

    fn rename(mut self, f: &dyn Fn(Name) -> Name) -> Self {
        self.rename_mut(&|name| {
            *name = f(*name);
        });
        self
    }
}
