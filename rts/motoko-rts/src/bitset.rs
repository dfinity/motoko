//! This module implements a simple bit set to be used by the compiler (in generated code)

use crate::idl_trap_with;

#[repr(packed)]
pub struct BitSet {
    /// Pointer into the bit set
    pub ptr: *mut u8,
    /// Pointer to the end of the bit set
    pub end: *mut u8,
}

impl BitSet {
    #[cfg(feature = "ic")]
    pub(crate) unsafe fn set(self: *mut Self, n: u32) {
        let byte = (n / 8) as usize;
        let bit = (n % 8) as u8;
        let dst = (*self).ptr.add(byte);
        if dst > (*self).end {
            idl_trap_with("BitSet.set out of bounds");
        };
        *dst = *dst | (1 << bit);
    }

    pub(crate) unsafe fn get(self: *mut Self, n: u32) -> bool {
        let byte = (n / 8) as usize;
        let bit = (n % 8) as u8;
        let src = (*self).ptr.add(byte);
        if src > (*self).end {
            idl_trap_with("BitSet.get out of bounds");
        };
        let mask = 1 << bit;
        return *src & mask == mask;
    }
}
