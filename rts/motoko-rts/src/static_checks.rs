//! Compile-time assertions to make sure object layouts are as expected

use crate::types::*;

use core::mem::{align_of, size_of};

// `_` suppresses "unused X" warnings so we don't get any warnings for the code below, but they use
// `WORD_SIZE` so we get an "unused constant WORD_SIZE" warning without `allow(unused)` here.
#[allow(unused)]
const WORD_SIZE: usize = crate::constants::WORD_SIZE as usize;

// We cannot use `assert_eq` below as `assert_eq` is not const yet

// Check platform word size
const _: () = assert!(size_of::<usize>() == size_of::<u32>());
const _: () = assert!(size_of::<usize>() == WORD_SIZE);

// Check that sizes of structs are as expected by the compiler
// (Expectations are all over the place, e.g. `header_size` definitions in `compile.ml`, calls to `static_closure`, etc.)
const _: () = assert!(size_of::<Obj>() == 1 * WORD_SIZE);
const _: () = assert!(size_of::<ObjInd>() == 2 * WORD_SIZE);
const _: () = assert!(size_of::<Closure>() == 3 * WORD_SIZE);
const _: () = assert!(size_of::<Blob>() == 2 * WORD_SIZE);
const _: () = assert!(size_of::<BigInt>() == 5 * WORD_SIZE);
const _: () = assert!(size_of::<MutBox>() == 2 * WORD_SIZE);
const _: () = assert!(size_of::<Some>() == 2 * WORD_SIZE);
const _: () = assert!(size_of::<Variant>() == 3 * WORD_SIZE);
const _: () = assert!(size_of::<Concat>() == 4 * WORD_SIZE);
const _: () = assert!(size_of::<Null>() == 1 * WORD_SIZE);
const _: () = assert!(size_of::<Bits32>() == 2 * WORD_SIZE);
const _: () = assert!(size_of::<Bits64>() == 3 * WORD_SIZE);

// These aren't used generated by the compiler
const _: () = assert!(size_of::<OneWordFiller>() == 1 * WORD_SIZE);
const _: () = assert!(size_of::<FreeSpace>() == 2 * WORD_SIZE);
const _: () = assert!(size_of::<FwdPtr>() == 2 * WORD_SIZE);

// Check that objects need to be aligned on word boundaries. Having a different alignment
// restriction on object type would require changing allocation routines for it.
const _: () = assert!(align_of::<Obj>() == WORD_SIZE);
const _: () = assert!(align_of::<ObjInd>() == WORD_SIZE);
const _: () = assert!(align_of::<Closure>() == WORD_SIZE);
const _: () = assert!(align_of::<Blob>() == WORD_SIZE);
const _: () = assert!(align_of::<BigInt>() == WORD_SIZE);
const _: () = assert!(align_of::<MutBox>() == WORD_SIZE);
const _: () = assert!(align_of::<Some>() == WORD_SIZE);
const _: () = assert!(align_of::<Variant>() == WORD_SIZE);
const _: () = assert!(align_of::<Concat>() == WORD_SIZE);
const _: () = assert!(align_of::<Null>() == WORD_SIZE);
const _: () = assert!(align_of::<Bits32>() == WORD_SIZE);
const _: () = assert!(align_of::<Bits64>() == WORD_SIZE);
const _: () = assert!(align_of::<OneWordFiller>() == WORD_SIZE);
const _: () = assert!(align_of::<FreeSpace>() == WORD_SIZE);
const _: () = assert!(align_of::<FwdPtr>() == WORD_SIZE);
