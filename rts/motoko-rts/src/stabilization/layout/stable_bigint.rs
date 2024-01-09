use crate::bigint::mp_calloc;
use crate::memory::Memory;
use crate::stabilization::deserialization::stable_memory_access::StableMemoryAccess;
use crate::stabilization::layout::{checked_to_usize, write_padding_u64};
use crate::stabilization::serialization::stable_memory_stream::{
    ScanStream, StableMemoryStream, WriteStream,
};
use crate::tommath_bindings::mp_digit;
use crate::types::{size_of, BigInt, Bytes, Value, Words, TAG_BIGINT};

use super::{checked_to_u32, round_to_u64, Serializer, StableToSpace, StableValue, StaticScanner};

// Tom's math library, as configured for Motoko RTS, encodes a big numbers as an array of 32-bit
// words, where each word stores 28 bits of the number while its highest 4 bits are zero.
// Similar to the little endian encoding, the word array starts with the least significant 28 bits,
// with each subsequent element covering the next higher 28 bits.

// Tom's math library functions `mp_to_sbin` and `mp_from_sbin` imply temporary object allocations
// which could be problematic when memory is short.
// Therefore, a custom serialization to the defined portable stable bignum format is implemented.

#[repr(C)]
pub struct StableBigInt {
    is_negative: bool, // Sign bit.
    number_of_bits: Bits, // Number of used bits in the payload.

                       // Dynamically sized payload of the big integer stored in little endian format.
                       // Zero padding to align to the next `u64`.
}

const USED_BITS_PER_WORD: u32 = 28;

#[repr(C)]
struct Bits(u64);

impl Bits {
    fn to_bytes(&self) -> Bytes<u32> {
        let bytes = ceiling_div(self.0, u8::BITS as u64);
        Bytes(checked_to_u32(bytes))
    }
}

impl StableBigInt {
    fn is_negative(main_object: *mut BigInt) -> bool {
        unsafe { (*main_object).mp_int.sign != 0 }
    }

    fn serialized_length(main_object: *mut BigInt) -> Bits {
        let used_words = unsafe { (*main_object).mp_int.used } as usize;
        if used_words == 0 {
            return Bits(0);
        }
        let last_word = unsafe { *main_object.payload_addr().add(used_words - 1) };
        debug_assert_ne!(last_word, 0);
        debug_assert_eq!(last_word >> USED_BITS_PER_WORD, 0);
        let last_bits = u32::BITS - last_word.leading_zeros();
        Bits((used_words - 1) as u64 * USED_BITS_PER_WORD as u64 + last_bits as u64)
    }

    fn deserialized_length(&self) -> Words<u32> {
        let words = ceiling_div(self.number_of_bits.0, USED_BITS_PER_WORD as u64);
        Words(checked_to_u32(words))
    }
}

fn ceiling_div(dividend: u64, divisor: u64) -> u64 {
    (dividend + divisor - 1) / divisor
}

impl StaticScanner<StableValue> for StableBigInt {}

impl Serializer<BigInt> for StableBigInt {
    unsafe fn serialize_static_part(main_object: *mut BigInt) -> Self {
        let is_negative = Self::is_negative(main_object);
        let number_of_bits = Self::serialized_length(main_object);
        StableBigInt {
            is_negative,
            number_of_bits,
        }
    }

    unsafe fn serialize_dynamic_part(
        stable_memory: &mut StableMemoryStream,
        main_object: *mut BigInt,
    ) {
        let total_words = (*main_object).mp_int.used as usize;
        let total_bytes = Self::serialized_length(main_object).to_bytes().as_usize();
        let payload = main_object.payload_addr();
        let mut written_bytes = 0;
        let mut word_index = 0;
        let mut word = 0;
        let mut pending_bits = 0;
        let mut next_word = 0;
        let mut next_pending_bits = 0;

        while written_bytes < total_bytes {
            while pending_bits < u32::BITS && word_index < total_words {
                debug_assert_eq!(next_pending_bits, 0);
                next_word = *payload.add(word_index);
                word_index += 1;
                debug_assert_eq!(next_word >> USED_BITS_PER_WORD, 0);
                word |= next_word << pending_bits;
                let consumed_bits = core::cmp::min(USED_BITS_PER_WORD, u32::BITS - pending_bits);
                pending_bits += consumed_bits;
                next_word >>= consumed_bits;
                next_pending_bits = USED_BITS_PER_WORD - consumed_bits;
            }
            stable_memory.write(&word); // little endian
            written_bytes += size_of::<u32>().to_bytes().as_usize();
            word = next_word;
            pending_bits = next_pending_bits;
            next_pending_bits = 0;
        }
        debug_assert!(pending_bits == 0 || word == 0);
        debug_assert_eq!(word_index, total_words);
        write_padding_u64(stable_memory, written_bytes);
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        &self,
        context: &mut C,
        _translate: &F,
    ) {
        let rounded_length = round_to_u64(self.number_of_bits.to_bytes().as_usize() as u64);
        context.to_space().skip(checked_to_usize(rounded_length));
    }

    unsafe fn allocate_deserialized<M: Memory>(&self, main_memory: &mut M) -> Value {
        let word_size = size_of::<u32>().to_bytes();
        let words = self.deserialized_length();
        let payload =
            mp_calloc(main_memory, words.as_usize(), Bytes(word_size.as_usize())) as *mut mp_digit;
        let bigint = BigInt::from_payload(payload);
        Value::from_ptr(bigint as usize)
    }

    unsafe fn deserialize_static_part(&self, target_bigint: *mut BigInt) {
        debug_assert_eq!((*target_bigint).header.tag, TAG_BIGINT);
        let deseserialized_bytes = self.deserialized_length().as_u32();
        debug_assert_eq!((*target_bigint).mp_int.alloc as u32, deseserialized_bytes);
        (*target_bigint).mp_int.sign = if self.is_negative { 1 } else { 0 };
        (*target_bigint).mp_int.used = deseserialized_bytes as i32;
    }

    unsafe fn deserialize_dynamic_part<M: Memory>(
        &self,
        _main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_bigint: *mut BigInt,
    ) {
        let stable_address = stable_object.payload_address();
        let source_payload =
            stable_address + size_of::<StableBigInt>().to_bytes().as_usize() as u64;
        let target_payload = target_bigint.payload_addr();
        let total_bytes = self.number_of_bits.to_bytes().as_usize();
        let total_words = self.deserialized_length().as_usize();

        let mut read_offset = 0;
        let mut word_index = 0;
        let mut word = 0;
        let mut pending_bits = 0;
        let mut next_byte = 0;
        let mut next_pending_bits = 0;

        while word_index < total_words {
            while pending_bits < USED_BITS_PER_WORD && read_offset < total_bytes {
                debug_assert_eq!(next_pending_bits, 0);
                next_byte = stable_memory.read::<u8>(source_payload + read_offset as u64);
                read_offset += 1;
                word |= (next_byte as u32) << pending_bits;
                word &= (1 << USED_BITS_PER_WORD) - 1;
                let consumed_bits = core::cmp::min(u8::BITS, USED_BITS_PER_WORD - pending_bits);
                pending_bits += consumed_bits;
                next_byte = if consumed_bits < u8::BITS {
                    next_byte >> consumed_bits
                } else {
                    0
                };
                next_pending_bits = u8::BITS - consumed_bits;
            }
            *target_payload.add(word_index) = word; // little endian
            word_index += 1;
            word = next_byte as u32;
            pending_bits = next_pending_bits;
            next_pending_bits = 0;
        }

        debug_assert!(pending_bits == 0 || word == 0);
        debug_assert_eq!(word_index, self.deserialized_length().as_usize());
    }
}
