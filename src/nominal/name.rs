use lazy_static::lazy_static;
use std::sync::atomic::AtomicU64;

lazy_static! {
    static ref COUNTER: AtomicU64 = AtomicU64::new(0);
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Name {
    value: u64,
}

pub fn fresh() -> Name {
    Name {
        value: COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
    }
}
