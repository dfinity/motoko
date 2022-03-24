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
    pub(crate) unsafe fn words(size1: u32, size2: u32) -> u32 {
        return ((2 * size1 * size2 * BITS) + (usize::BITS - 1)) / usize::BITS;
    }

    pub(crate) unsafe fn init(self: &Self) {
        let bytes = ((self.end as usize) - (self.ptr as usize)) as u32;
        if (2 * self.size1 * self.size2 * BITS) > bytes * 8 {
            idl_trap_with("BitRel not enough bytes");
        };
        memzero(self.ptr as usize, Words(bytes / WORD_SIZE));
    }

    pub(crate) unsafe fn set(self: &Self, p: bool, i_j: u32, j_i: u32, bit: u32, v: bool) {
        let size1 = self.size1;
        let size2 = self.size2;
        let (base, i, j) = if p {
            (0, i_j, j_i)
        } else {
            (size1 * size2 * BITS, j_i, i_j)
        };
        if i >= size1 {
            idl_trap_with("BitRel.set i out of bounds");
        };
        if j >= size2 {
            idl_trap_with("BitRel.set j out of bounds");
        };
        if bit >= BITS {
            idl_trap_with("BitRel.set bit out of bounds");
        };
        let k = base + i * size2 * BITS + j + bit;
        let word = (k / usize::BITS) as usize;
        let bit = (k % usize::BITS) as u32;
        let dst = self.ptr.add(word);
        if dst > self.end {
            idl_trap_with("BitRel.set out of bounds");
        };
        if v {
            *dst = *dst | (1 << bit);
        } else {
            *dst = *dst & !(1 << bit);
        }
    }

    pub(crate) unsafe fn get(self: &Self, p: bool, i_j: u32, j_i: u32, bit: u32) -> bool {
        let size1 = self.size1;
        let size2 = self.size2;
        let (base, i, j) = if p {
            (0, i_j, j_i)
        } else {
            (size1 * size2 * BITS, j_i, i_j)
        };
        if i >= size1 {
            idl_trap_with("BitRel.get i out of bounds");
        };
        if j >= size2 {
            idl_trap_with("BitRel.get j out of bounds");
        };
        if bit >= BITS {
            idl_trap_with("BitRel.get bit out of bounds");
        };
        let k = base + i * size2 * BITS + j + bit;
        let word = (k / usize::BITS) as usize;
        let bit = (k % usize::BITS) as u32;
        let src = self.ptr.add(word);
        if src > self.end {
            idl_trap_with("BitRel.get out of bounds");
        };
        let mask = 1 << bit;
        return *src & mask == mask;
    }
}
