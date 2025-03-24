#[enhanced_orthogonal_persistence]
pub mod enhanced;

#[classical_persistence]
pub mod classical;

use motoko_rts_macros::{classical_persistence, enhanced_orthogonal_persistence};

use crate::types::Value;

#[enhanced_orthogonal_persistence]
pub type Roots = self::enhanced::Roots;

#[classical_persistence]
pub type Roots = self::classical::Roots;

#[cfg(feature = "ic")]
#[enhanced_orthogonal_persistence]
pub unsafe fn root_set() -> Roots {
    self::enhanced::root_set()
}

#[cfg(feature = "ic")]
#[classical_persistence]
pub unsafe fn root_set() -> Roots {
    self::classical::root_set()
}

#[enhanced_orthogonal_persistence]
pub unsafe fn visit_roots<C, V: Fn(&mut C, *mut Value)>(
    roots: Roots,
    _heap_base: usize, // Only used with classical persistence.
    context: &mut C,
    visit_field: V,
) {
    self::enhanced::visit_roots(roots, context, visit_field);
}

#[classical_persistence]
pub unsafe fn visit_roots<C, V: Fn(&mut C, *mut Value)>(
    roots: Roots,
    heap_base: usize, // Only used with classical persistence.
    context: &mut C,
    visit_field: V,
) {
    self::classical::visit_roots(roots, heap_base, context, visit_field);
}
