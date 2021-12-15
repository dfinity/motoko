//! Static assertions to make sure object layouts are as expected

use crate::types::*;

use core::mem::{align_of, size_of};

use static_assertions::const_assert_eq;

// `const_assert_eq` call below are expanded into something like `const _: ... = ...`. Because of
// `_` they don't generate "unused constant" warnings, but the constants used in the RHSs are
// considered unused, so we get a "unused constant" warning for `WORD_SIZE`.
//
// Example:
//
//     const X: u32 = 123;
//     const _: u32 = X;
//
// Code above generates "unused constant X" warning.
#[allow(unused)]
const WORD_SIZE: usize = crate::constants::WORD_SIZE as usize;

// Check platform word size
const_assert_eq!(size_of::<usize>(), size_of::<u32>());
const_assert_eq!(size_of::<usize>(), WORD_SIZE);

// Check that sizes of structs are as expected by the compiler
// (Expectations are all over the place, e.g. `header_size` definitions in `compile.ml`, calls to `static_closure`, etc.)
const_assert_eq!(size_of::<Obj>(), 1 * WORD_SIZE);
const_assert_eq!(size_of::<ObjInd>(), 2 * WORD_SIZE);
const_assert_eq!(size_of::<Closure>(), 3 * WORD_SIZE);
const_assert_eq!(size_of::<Blob>(), 2 * WORD_SIZE);
const_assert_eq!(size_of::<BigInt>(), 5 * WORD_SIZE);
const_assert_eq!(size_of::<MutBox>(), 2 * WORD_SIZE);
const_assert_eq!(size_of::<Some>(), 2 * WORD_SIZE);
const_assert_eq!(size_of::<Variant>(), 3 * WORD_SIZE);
const_assert_eq!(size_of::<Concat>(), 4 * WORD_SIZE);
const_assert_eq!(size_of::<Null>(), 1 * WORD_SIZE);
const_assert_eq!(size_of::<Bits32>(), 2 * WORD_SIZE);
const_assert_eq!(size_of::<Bits64>(), 3 * WORD_SIZE);

// These aren't used generated by the compiler
const_assert_eq!(size_of::<OneWordFiller>(), 1 * WORD_SIZE);
const_assert_eq!(size_of::<FreeSpace>(), 2 * WORD_SIZE);
const_assert_eq!(size_of::<FwdPtr>(), 2 * WORD_SIZE);

// Check that objects need to be aligned on word boundaries. Having a different alignment
// restriction an object type would require changing allocation routines for it.
const_assert_eq!(align_of::<Obj>(), WORD_SIZE);
const_assert_eq!(align_of::<ObjInd>(), WORD_SIZE);
const_assert_eq!(align_of::<Closure>(), WORD_SIZE);
const_assert_eq!(align_of::<Blob>(), WORD_SIZE);
const_assert_eq!(align_of::<BigInt>(), WORD_SIZE);
const_assert_eq!(align_of::<MutBox>(), WORD_SIZE);
const_assert_eq!(align_of::<Some>(), WORD_SIZE);
const_assert_eq!(align_of::<Variant>(), WORD_SIZE);
const_assert_eq!(align_of::<Concat>(), WORD_SIZE);
const_assert_eq!(align_of::<Null>(), WORD_SIZE);
const_assert_eq!(align_of::<Bits32>(), WORD_SIZE);
const_assert_eq!(align_of::<Bits64>(), WORD_SIZE);
const_assert_eq!(align_of::<OneWordFiller>(), WORD_SIZE);
const_assert_eq!(align_of::<FreeSpace>(), WORD_SIZE);
const_assert_eq!(align_of::<FwdPtr>(), WORD_SIZE);
