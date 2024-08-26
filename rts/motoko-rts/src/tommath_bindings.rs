#![allow(unused, non_camel_case_types)]

use motoko_rts_macros::{classical_persistence, enhanced_orthogonal_persistence};

#[classical_persistence]
include!("../../_build/wasm32/tommath_bindings.rs");

#[enhanced_orthogonal_persistence]
include!("../../_build/wasm64/tommath_bindings.rs");
