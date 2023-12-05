use crate::barriers::allocation_barrier;
use crate::bigint::{check, persist_bigint, tmp_bigint};
use crate::memory::{alloc_blob, Memory};
use crate::stabilization::layout::{checked_to_usize, write_padding_u64};
use crate::stabilization::stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream};
use crate::stabilization::StableMemoryAccess;
use crate::tommath_bindings::{mp_from_sbin, mp_int, mp_sbin_size, mp_to_sbin};
use crate::types::{size_of, BigInt, Bytes, Value};

use super::{round_to_u64, Serializer, StableToSpace, StableValue, StaticScanner};

#[repr(C)]
pub struct StableBigInt {
    binary_length: u32, // Number of payload bytes.
                        // Dynamically sized payload of the big integer stored in Big-endian format.
                        // Zero padding to align to next `u64`.
}

impl StableBigInt {
    fn binary_length(main_object: *mut BigInt) -> usize {
        unsafe { mp_sbin_size(main_object.mp_int_ptr()) }
    }

    fn create_buffer<M: Memory>(mem: &mut M, size: usize) -> *mut u8 {
        unsafe {
            let size = Bytes(size as u32);
            let blob = alloc_blob(mem, size);
            let buffer = blob.as_blob_mut().payload_addr();
            // No buffer initialization by `mp_to_sbin` and `mp_from_sbin`.
            allocation_barrier(blob);
            buffer
        }
    }
}

impl StaticScanner<StableValue> for StableBigInt {}

impl Serializer<BigInt> for StableBigInt {
    unsafe fn serialize_static_part(main_object: *mut BigInt) -> Self {
        let binary_length = Self::binary_length(main_object) as u32;
        StableBigInt { binary_length }
    }

    unsafe fn serialize_dynamic_part<M: Memory>(
        main_memory: &mut M,
        stable_memory: &mut StableMemoryStream,
        main_object: *mut BigInt,
    ) {
        let binary_length = Self::binary_length(main_object);
        let buffer = Self::create_buffer(main_memory, binary_length);
        let mut written = 0;
        check(mp_to_sbin(
            main_object.mp_int_ptr(),
            buffer,
            binary_length,
            &mut written as *mut usize,
        ));
        assert_eq!(written, binary_length);
        stable_memory.raw_write(buffer as usize, binary_length);
        write_padding_u64(stable_memory, binary_length);
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        &self,
        context: &mut C,
        _translate: &F,
    ) {
        let rounded_length = round_to_u64(self.binary_length as u64);
        context.to_space().skip(checked_to_usize(rounded_length));
    }

    unsafe fn deserialize<M: Memory>(
        main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        let stable_address = stable_object.payload_address();
        let stable_static_part = stable_memory.read::<Self>(stable_address);
        let binary_length = stable_static_part.binary_length as usize;
        let buffer = Self::create_buffer(main_memory, binary_length);
        let source_payload =
            stable_address + size_of::<StableBigInt>().to_bytes().as_usize() as u64;
        stable_memory.raw_read(source_payload, buffer as usize, binary_length);
        let mut temporary = tmp_bigint();
        check(mp_from_sbin(
            &mut temporary as *mut mp_int,
            buffer,
            binary_length,
        ));
        persist_bigint(temporary)
    }

    unsafe fn deserialize_static_part(&self, _target_object: *mut BigInt) {
        unreachable!()
    }
}
