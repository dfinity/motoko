#![allow(unused, non_camel_case_types)]

use motoko_rts_macros::{classical_persistence, enhanced_orthogonal_persistence};

#[enhanced_orthogonal_persistence]
include!("../../_build/tommath_bindings_64.rs");

#[classical_persistence]
include!("../../_build/tommath_bindings_32.rs");
