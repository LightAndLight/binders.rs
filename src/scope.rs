use super::name::Name;

pub struct Scope;

impl Scope {
    pub fn name<'a>(&'a self, name: &'a Name) -> Name {
        Name { value: name.value }
    }
}
