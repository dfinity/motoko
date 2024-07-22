//! Customized option type with a fixed C-representation.
//! As the native Rust type may change in future Rust releases,
//! the Motoko runtime system uses its own dedicated `StableOption`
//! type with a fixed long-term representation. This is used in the
//! persistent metadata, such as in the garbage collector state.
//! See:
//! * `gc::incremental::State`
//! * `gc::partitioned_heap::PartitionedHeapIterator`

/// Stable option type used for orthogonal persistent state.
/// Analogous use like the Rust-native `Option`.
#[repr(C)]
pub enum StableOption<T> {
    None,
    Some(T),
}

impl<T> StableOption<T> {
    pub const fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            Self::Some(_) => false,
        }
    }

    pub const fn is_some(&self) -> bool {
        match self {
            Self::None => false,
            Self::Some(_) => true,
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Self::None => panic!("Unwrapping `None` on `StableOption`"),
            Self::Some(x) => x,
        }
    }

    pub fn as_mut(&mut self) -> StableOption<&mut T> {
        match *self {
            Self::Some(ref mut x) => StableOption::Some(x),
            Self::None => StableOption::None,
        }
    }

    pub const fn as_ref(&self) -> StableOption<&T> {
        match *self {
            Self::Some(ref x) => StableOption::Some(x),
            Self::None => StableOption::None,
        }
    }
}
