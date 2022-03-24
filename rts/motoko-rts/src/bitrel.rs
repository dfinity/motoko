//! This module implements a simple subtype cache used by the compiler (in generated code)

use crate::constants::WORD_SIZE;
use crate::idl_trap_with;
use crate::mem_utils::memzero;
use crate::types::Words;

const BITS: u32 = 2;

#[repr(packed)]
pub struct BitRel {
    /// Pointer into the bit set
    pub ptr: *mut u32,
    /// Pointer to the end of the bit set
    /// must allow at least 2 * size1 * size2 bits
    pub end: *mut u32,
    pub size1: u32,
    pub size2: u32,
}

impl BitRel {
    pub(crate) fn words(size1: u32, size2: u32) -> u32 {
        return ((2 * size1 * size2 * BITS) + (usize::BITS - 1)) / usize::BITS;
    }

    pub(crate) unsafe fn init(&self) {
        let bytes = ((self.end as usize) - (self.ptr as usize)) as u32;
        if (2 * self.size1 * self.size2 * BITS) > bytes * 8 {
            idl_trap_with("BitRel not enough bytes");
        };
        memzero(self.ptr as usize, Words(bytes / WORD_SIZE));
    }

    unsafe fn locate_ptr_bit(
        self: &Self,
        p: bool,
        i_j: u32,
        j_i: u32,
        bit: u32,
    ) -> (*mut u32, u32) {
        let size1 = self.size1;
        let size2 = self.size2;
        let (base, i, j) = if p {
            (0, i_j, j_i)
        } else {
            (size1 * size2 * BITS, j_i, i_j)
        };
        if i >= size1 {
            idl_trap_with("BitRel i out of bounds");
        };
        if j >= size2 {
            idl_trap_with("BitRel j out of bounds");
        };
        if bit >= BITS {
            idl_trap_with("BitRel bit out of bounds");
        };
        let k = base + i * size2 * BITS + j + bit;
        let word = (k / usize::BITS) as usize;
        let bit = (k % usize::BITS) as u32;
        let ptr = self.ptr.add(word);
        if ptr > self.end {
            idl_trap_with("BitRel ptr out of bounds");
        };
        return (ptr, bit);
    }

    pub(crate) unsafe fn set(&self, p: bool, i_j: u32, j_i: u32, bit: u32, v: bool) {
        let (ptr, bit) = self.locate_ptr_bit(p, i_j, j_i, bit);
        if v {
            *ptr = *ptr | (1 << bit);
        } else {
            *ptr = *ptr & !(1 << bit);
        }
    }

    pub(crate) unsafe fn get(&self, p: bool, i_j: u32, j_i: u32, bit: u32) -> bool {
        let (ptr, bit) = self.locate_ptr_bit(p, i_j, j_i, bit);
        let mask = 1 << bit;
        return *ptr & mask == mask;
    }
}
