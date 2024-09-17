//! This module implements a simple subtype cache used by the compiler (in generated code)

use crate::constants::WORD_SIZE;
use crate::idl_trap_with;
use crate::mem_utils::memzero;
use crate::types::Words;

const BITS: usize = 2;

#[repr(packed)]
pub struct BitRel {
    /// Pointer into the bit set
    pub ptr: *mut usize,
    /// Pointer to the end of the bit set
    /// must allow at least 2 * size1 * size2 bits
    pub end: *mut usize,
    pub size1: usize,
    pub size2: usize,
}

impl BitRel {
    pub fn words(size1: usize, size2: usize) -> usize {
        return ((2 * size1 * size2 * BITS) + (usize::BITS as usize - 1)) / usize::BITS as usize;
    }

    pub unsafe fn init(&self) {
        if (self.end as usize) < (self.ptr as usize) {
            idl_trap_with("BitRel invalid fields");
        };

        let bytes = (self.end as usize) - (self.ptr as usize);
        if bytes != BitRel::words(self.size1, self.size2) * WORD_SIZE {
            idl_trap_with("BitRel missized");
        };
        memzero(self.ptr as usize, Words(bytes / WORD_SIZE));
    }

    unsafe fn locate_ptr_bit(
        &self,
        p: bool,
        i_j: usize,
        j_i: usize,
        bit: usize,
    ) -> (*mut usize, usize) {
        let size1 = self.size1;
        let size2 = self.size2;
        let (base, i, j) = if p { (0, i_j, j_i) } else { (size1, j_i, i_j) };
        debug_assert!(i < size1);
        debug_assert!(j < size2);
        debug_assert!(bit < BITS);
        let k = ((base + i) * size2 + j) * BITS + bit;
        let word = k / (usize::BITS as usize);
        let bit = k % (usize::BITS as usize);
        let ptr = self.ptr.add(word);
        if ptr > self.end {
            idl_trap_with("BitRel indices out of bounds");
        };
        return (ptr, bit);
    }

    unsafe fn set(&self, p: bool, i_j: usize, j_i: usize, bit: usize, v: bool) {
        let (ptr, bit) = self.locate_ptr_bit(p, i_j, j_i, bit);
        if v {
            *ptr = *ptr | (1 << bit);
        } else {
            *ptr = *ptr & !(1 << bit);
        }
    }

    unsafe fn get(&self, p: bool, i_j: usize, j_i: usize, bit: usize) -> bool {
        let (ptr, bit) = self.locate_ptr_bit(p, i_j, j_i, bit);
        let mask = 1 << bit;
        return *ptr & mask == mask;
    }

    pub unsafe fn visited(&self, p: bool, i_j: usize, j_i: usize) -> bool {
        self.get(p, i_j, j_i, 0)
    }

    pub unsafe fn visit(&self, p: bool, i_j: usize, j_i: usize) {
        self.set(p, i_j, j_i, 0, true)
    }

    #[allow(dead_code)]
    // NB: we store related bits in negated form to avoid setting on assumption
    // This code is a nop in production code.
    pub unsafe fn assume(&self, p: bool, i_j: usize, j_i: usize) {
        debug_assert!(!self.get(p, i_j, j_i, 1));
    }

    pub unsafe fn related(&self, p: bool, i_j: usize, j_i: usize) -> bool {
        !self.get(p, i_j, j_i, 1)
    }

    pub unsafe fn disprove(&self, p: bool, i_j: usize, j_i: usize) {
        self.set(p, i_j, j_i, 1, true)
    }
}
