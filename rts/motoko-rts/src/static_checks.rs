//! Compile-time assertions to make sure object layouts are as expected

use crate::types::*;

use core::mem::{align_of, size_of};

// `_` suppresses "unused X" warnings so we don't get any warnings for the code below, but they use
// `WORD_SIZE` so we get an "unused constant WORD_SIZE" warning without `allow(unused)` here.
#[allow(unused)]
const WORD_SIZE: usize = crate::constants::WORD_SIZE as usize;

#[allow(unused)]
const HEADER_SIZE: usize = 2 * WORD_SIZE;

// We cannot use `assert_eq` below as `assert_eq` is not const yet

// Check platform word size
const _: () = assert!(size_of::<u32>() == WORD_SIZE);

// Check that sizes of structs are as expected by the compiler
// (Expectations are all over the place, e.g. `header_size` definitions in `compile.ml`, calls to `static_closure`, etc.)
const _: () = assert!(size_of::<Obj>() == HEADER_SIZE);
const _: () = assert!(size_of::<ObjInd>() == HEADER_SIZE + 1 * WORD_SIZE);
const _: () = assert!(size_of::<Closure>() == HEADER_SIZE + 2 * WORD_SIZE);
const _: () = assert!(size_of::<Blob>() == HEADER_SIZE + 2 * WORD_SIZE);
const _: () = assert!(size_of::<BigInt>() == HEADER_SIZE + 6 * WORD_SIZE);
const _: () = assert!(size_of::<MutBox>() == HEADER_SIZE + 1 * WORD_SIZE);
const _: () = assert!(size_of::<Some>() == HEADER_SIZE + 1 * WORD_SIZE);
const _: () = assert!(size_of::<Variant>() == HEADER_SIZE + 2 * WORD_SIZE);
const _: () = assert!(size_of::<Concat>() == HEADER_SIZE + 4 * WORD_SIZE);
const _: () = assert!(size_of::<Null>() == HEADER_SIZE);
const _: () = assert!(size_of::<Bits32>() == HEADER_SIZE + 1 * WORD_SIZE);
const _: () = assert!(size_of::<Bits64>() == HEADER_SIZE + 2 * WORD_SIZE);

// Not used by the compiler backend.
const _: () = assert!(size_of::<OneWordFiller>() == 1 * WORD_SIZE);
const _: () = assert!(size_of::<FreeSpace>() == 4 * WORD_SIZE);
const _: () = assert!(size_of::<FwdPtr>() == 2 * WORD_SIZE);

// Check that objects are aligned to at least the platform bitwidth.
const _: () = assert!(correctly_aligned::<Obj>());
const _: () = assert!(correctly_aligned::<ObjInd>());
const _: () = assert!(correctly_aligned::<Closure>());
const _: () = assert!(correctly_aligned::<Blob>());
const _: () = assert!(correctly_aligned::<BigInt>());
const _: () = assert!(correctly_aligned::<MutBox>());
const _: () = assert!(correctly_aligned::<Some>());
const _: () = assert!(correctly_aligned::<Variant>());
const _: () = assert!(correctly_aligned::<Concat>());
const _: () = assert!(correctly_aligned::<Null>());
const _: () = assert!(correctly_aligned::<Bits32>());
const _: () = assert!(correctly_aligned::<Bits64>());
const _: () = assert!(correctly_aligned::<OneWordFiller>());
const _: () = assert!(correctly_aligned::<FreeSpace>());
const _: () = assert!(correctly_aligned::<FwdPtr>());

#[allow(unused)]
const fn correctly_aligned<T>() -> bool {
    size_of::<usize>() % align_of::<T>() == 0
}
