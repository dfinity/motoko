use core::mem::zeroed;

use crate::stabilization::layout::{checked_to_usize, write_padding_u64};
use crate::stabilization::reader_writer::{ScanStream, StableMemorySpace, WriteStream};
use crate::stabilization::StableMemoryAccess;
use crate::tommath_bindings::{mp_digit, mp_int};
use crate::types::{BigInt, Bytes, Obj, Value, TAG_BIGINT};

use super::{checked_to_u32, round_to_u64, Serializer, StableValue, StaticScanner};

// TODO: Use a format that is independent of Tom's math library, e.g. by using LEB128/SLEB128 encoding.
// Redesign `bigint.rs` to
// * Stream to stable memory without needing a temporary buffer.
// * Compute the required size of `mp_int` without writing to heap.
//  (This is required by the deserialization algorithm that streams the main memory layout to stable memory
//  and uses allocators to only compute the target addresses.)

#[repr(C)]
pub struct StableBigInt {
    mp_int: mp_int,
    // Dynamically sized payload of `BigInt::data_length` bytes.
    // Zero padding to align to next `u64`.
}

impl StableBigInt {
    fn length(&self) -> u64 {
        let mp_int = &self.mp_int as *const mp_int;
        unsafe { BigInt::data_length(mp_int).as_u32() as u64 }
    }

    unsafe fn payload_address(self: *mut Self) -> *mut mp_digit {
        self.add(1) as *mut mp_digit
    }

    unsafe fn payload_length(self: *const Self) -> u64 {
        (*self).length()
    }
}

impl StaticScanner<StableValue> for StableBigInt {}
impl StaticScanner<Value> for BigInt {}

impl Default for StableBigInt {
    fn default() -> Self {
        StableBigInt {
            mp_int: unsafe { zeroed() },
        }
    }
}

impl Default for BigInt {
    fn default() -> Self {
        Self {
            header: Default::default(),
            mp_int: unsafe { zeroed() },
        }
    }
}

impl Serializer<BigInt> for StableBigInt {
    unsafe fn serialize_static_part(main_object: *mut BigInt) -> Self {
        StableBigInt {
            mp_int: (*main_object).mp_int,
        }
    }

    unsafe fn serialize_dynamic_part(memory: &mut StableMemorySpace, main_object: *mut BigInt) {
        let byte_length = main_object.len().as_usize();
        memory.raw_write(main_object.payload_addr() as usize, byte_length);
        write_padding_u64(memory, byte_length);
    }

    fn scan_serialized_dynamic<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        context: &mut C,
        stable_object: &Self,
        _translate: &F,
    ) {
        let rounded_length = round_to_u64(stable_object.length());
        context.to_space().skip(checked_to_usize(rounded_length));
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> BigInt {
        BigInt {
            header: Obj::new(TAG_BIGINT, target_address),
            mp_int: stable_object.read_unaligned().mp_int,
        }
    }

    unsafe fn deserialize_dynamic_part(memory: &mut StableMemorySpace, stable_object: *mut Self) {
        let byte_length = checked_to_u32(stable_object.payload_length());
        let rounded_length = Bytes(byte_length).to_words().to_bytes().as_usize();
        memory.raw_write(stable_object.payload_address() as usize, rounded_length)
    }

    fn scan_deserialized_dynamic<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        context: &mut C,
        main_object: &BigInt,
        _translate: &F,
    ) {
        let byte_length = unsafe { BigInt::data_length(&main_object.mp_int as *const mp_int) };
        let rounded_length = byte_length.to_words().to_bytes().as_usize();
        context.to_space().skip(rounded_length);
    }
}
