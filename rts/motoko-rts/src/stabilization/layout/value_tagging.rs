//! Value tagging scheme used by the stable format of the graph-copy-based serialization.
//! The tags provide runtime type information for values stored in object fields and array elements.
//! It allows distinguishing between pointer and scalar types and specifies the exact scalar type.
//!
//! The tagging scheme used by the stable format is based on 64-bit encoding and is different
//! to the current compiler and runtime system. This for easing future upgrades to 64-bit main memory
//! by way of the graph copy.
//!
//! Stable value encoding:
//! `0`, `1` define the bitmash of the tag, `.` represents a payload bit.
//!
//! Type                | Encoding                                                                    | Payload length     | Note
//! --------------------|-----------------------------------------------------------------------------|--------------------|---------------------------------------------------------------------------
//! Boolean `false`     | 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000   | 0                  |
//! Boolean `true`      | 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001   | 0                  |
//! Pointer             | 0b1......._........_........_........_........_........_........_.......1   | 63                 | Address is divided by two and skewed. Most-significant bit is `1`.
//! `Int` or `Nat`      | 0b........_........_........_........_........_........_........_......10   | 62                 | Larger payloads need boxing. `Nat` are encoded as `Int` with sign bit `0`.
//! `Nat64`             | 0b........_........_........_........_........_........_........_....0100   | 60                 | Larger payloads need boxing.
//! `Int64`             | 0b........_........_........_........_........_........_........_....1100   | 60                 | Larger payloads need boxing.
//! `Nat32`             | 0b........_........_........_........_01000000_00000000_00000000_00000000   | 32                 |
//! `Int32`             | 0b........_........_........_........_11000000_00000000_00000000_00000000   | 32                 |
//! `Char`              | 0b........_........_.....010_00000000_00000000_00000000_00000000_00000000   | 21                 | 21 bit suffice to encode all possible UTF-8 code points.
//! `Nat16`             | 0b........_........_01000000_00000000_00000000_00000000_00000000_00000000   | 16                 |
//! `Int16`             | 0b........_........_11000000_00000000_00000000_00000000_00000000_00000000   | 16                 |
//! `Nat8`              | 0b........_01000000_00000000_00000000_00000000_00000000_00000000_00000000   | 8                  |
//! `Int8`              | 0b........_11000000_00000000_00000000_00000000_00000000_00000000_00000000   | 8                  |
//! `()` unit           | 0b01000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000   | 0                  |
//!
//!
//! Different to stable format, the tagging scheme of the runtime system and compiler version is
//! specified in `compile.ml`.

use crate::types::Value;

use super::{
    checked_to_i32, checked_to_u32, StableValue, STABLE_NULL_POINTER, STABLE_NULL_POINTER_32,
};

#[derive(Copy, Clone, Debug)]
enum ValueTag {
    FALSE,
    TRUE,
    BIGINT,
    NAT64,
    INT64,
    NAT32,
    INT32,
    CHAR,
    NAT16,
    INT16,
    NAT8,
    INT8,
    UNIT,
}

impl ValueTag {
    fn is_signed(&self) -> bool {
        match self {
            Self::BIGINT | Self::INT64 | Self::INT32 | Self::INT16 | Self::INT8 => true,
            Self::FALSE
            | Self::TRUE
            | Self::NAT64
            | Self::NAT32
            | Self::CHAR
            | Self::NAT16
            | Self::NAT8
            | Self::UNIT => false,
        }
    }

    fn has_payload(&self) -> bool {
        match self {
            Self::BIGINT
            | Self::NAT64
            | Self::INT64
            | Self::NAT32
            | Self::INT32
            | Self::CHAR
            | Self::NAT16
            | Self::INT16
            | Self::NAT8
            | Self::INT8 => true,
            Self::FALSE | Self::TRUE | Self::UNIT => false,
        }
    }
}

struct StableValueEncoding {
    tag: ValueTag,
    scalar: u64,
}

impl StableValueEncoding {
    fn tag_bits(tag: ValueTag) -> u64 {
        match tag {
            ValueTag::FALSE => {
                0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
            }
            ValueTag::TRUE => {
                0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001
            }
            ValueTag::BIGINT => 0b10,
            ValueTag::NAT64 => 0b0100,
            ValueTag::INT64 => 0b1100,
            ValueTag::NAT32 => 0b01000000_00000000_00000000_00000000,
            ValueTag::INT32 => 0b11000000_00000000_00000000_00000000,
            ValueTag::CHAR => 0b010_00000000_00000000_00000000_00000000_00000000,
            ValueTag::NAT16 => 0b01000000_00000000_00000000_00000000_00000000_00000000,
            ValueTag::INT16 => 0b11000000_00000000_00000000_00000000_00000000_00000000,
            ValueTag::NAT8 => 0b01000000_00000000_00000000_00000000_00000000_00000000_00000000,
            ValueTag::INT8 => 0b11000000_00000000_00000000_00000000_00000000_00000000_00000000,
            ValueTag::UNIT => {
                0b01000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
            }
        }
    }

    fn tag_length(tag: ValueTag) -> usize {
        match tag {
            ValueTag::FALSE | ValueTag::TRUE | ValueTag::UNIT => 64,
            ValueTag::BIGINT => 2,
            ValueTag::NAT64 | ValueTag::INT64 => 4,
            ValueTag::NAT32 | ValueTag::INT32 => 32,
            ValueTag::CHAR => 43,
            ValueTag::NAT16 | ValueTag::INT16 => 48,
            ValueTag::NAT8 | ValueTag::INT8 => 56,
        }
    }

    fn tag_bitmask(tag: ValueTag) -> u64 {
        let length = Self::tag_length(tag);
        if length == u64::BITS as usize {
            u64::MAX
        } else {
            (1u64 << length) - 1
        }
    }

    fn matches(value: StableValue, tag: ValueTag) -> bool {
        value.get_raw() & Self::tag_bitmask(tag) == Self::tag_bits(tag)
    }

    fn encode(&self) -> StableValue {
        let length = Self::tag_length(self.tag);
        let shifted = if length == u64::BITS as usize {
            0
        } else {
            self.scalar << length as u64
        };
        let tagged = shifted | Self::tag_bits(self.tag);
        StableValue::from_raw(tagged)
    }

    fn decode(value: StableValue) -> Self {
        let tag = match value.get_raw().trailing_zeros() {
            64 => ValueTag::FALSE,
            0 => ValueTag::TRUE,
            1 => ValueTag::BIGINT,
            2 if Self::matches(value, ValueTag::NAT64) => ValueTag::NAT64,
            2 if Self::matches(value, ValueTag::INT64) => ValueTag::INT64,
            30 if Self::matches(value, ValueTag::NAT32) => ValueTag::NAT32,
            30 if Self::matches(value, ValueTag::INT32) => ValueTag::INT32,
            41 => ValueTag::CHAR,
            46 if Self::matches(value, ValueTag::NAT16) => ValueTag::NAT16,
            46 if Self::matches(value, ValueTag::INT16) => ValueTag::INT16,
            54 if Self::matches(value, ValueTag::NAT8) => ValueTag::NAT8,
            54 if Self::matches(value, ValueTag::INT8) => ValueTag::INT8,
            62 => ValueTag::UNIT,
            _ => unreachable!("Invalid tag"),
        };
        debug_assert!(Self::matches(value, tag));
        let scalar = if tag.has_payload() {
            value.get_raw() >> (Self::tag_length(tag) as u64)
        } else {
            0
        };
        Self { tag, scalar }
    }
}

/// Value tags used by the current compiler and runtime system version.
/// See `compile.ml`.
struct RuntimeValueEncoding {
    tag: ValueTag,
    scalar: u32,
}

impl RuntimeValueEncoding {
    fn tag_bits(tag: ValueTag) -> u32 {
        match tag {
            ValueTag::FALSE => 0b00000000_00000000_00000000_00000000,
            ValueTag::TRUE => 0b00000000_00000000_00000000_00000001,
            ValueTag::BIGINT => 0b10,
            ValueTag::NAT64 => 0b0100,
            ValueTag::INT64 => 0b1100,
            ValueTag::NAT32 => 0b01000,
            ValueTag::INT32 => 0b11000,
            ValueTag::CHAR => 0b010_00000000,
            ValueTag::NAT16 => 0b01000000_00000000,
            ValueTag::INT16 => 0b11000000_00000000,
            ValueTag::NAT8 => 0b01000000_00000000_00000000,
            ValueTag::INT8 => 0b11000000_00000000_00000000,
            ValueTag::UNIT => 0b01000000_00000000_00000000_00000000,
        }
    }

    fn tag_length(tag: ValueTag) -> usize {
        match tag {
            ValueTag::FALSE | ValueTag::TRUE | ValueTag::UNIT => 32,
            ValueTag::BIGINT => 2,
            ValueTag::NAT64 | ValueTag::INT64 => 4,
            ValueTag::NAT32 | ValueTag::INT32 => 5,
            ValueTag::CHAR => 11,
            ValueTag::NAT16 | ValueTag::INT16 => 16,
            ValueTag::NAT8 | ValueTag::INT8 => 24,
        }
    }

    fn tag_bitmask(tag: ValueTag) -> u32 {
        let length = Self::tag_length(tag);
        if length == u32::BITS as usize {
            u32::MAX
        } else {
            (1u32 << length) - 1
        }
    }

    fn matches(value: Value, tag: ValueTag) -> bool {
        value.get_raw() & Self::tag_bitmask(tag) == Self::tag_bits(tag)
    }

    fn encode(&self) -> Value {
        let length = Self::tag_length(self.tag);
        let shifted = if length == u32::BITS as usize {
            0
        } else {
            self.scalar << length as u32
        };
        let tagged = shifted | Self::tag_bits(self.tag);
        Value::from_raw(tagged)
    }

    fn decode(value: Value) -> Self {
        let tag = match value.get_raw().trailing_zeros() {
            32 => ValueTag::FALSE,
            0 => ValueTag::TRUE,
            1 => ValueTag::BIGINT,
            2 if Self::matches(value, ValueTag::NAT64) => ValueTag::NAT64,
            2 if Self::matches(value, ValueTag::INT64) => ValueTag::INT64,
            3 if Self::matches(value, ValueTag::NAT32) => ValueTag::NAT32,
            3 if Self::matches(value, ValueTag::INT32) => ValueTag::INT32,
            9 => ValueTag::CHAR,
            14 if Self::matches(value, ValueTag::NAT16) => ValueTag::NAT16,
            14 if Self::matches(value, ValueTag::INT16) => ValueTag::INT16,
            22 if Self::matches(value, ValueTag::NAT8) => ValueTag::NAT8,
            22 if Self::matches(value, ValueTag::INT8) => ValueTag::INT8,
            30 => ValueTag::UNIT,
            _ => unreachable!("Invalid tag"),
        };
        debug_assert!(Self::matches(value, tag));
        let scalar = if tag.has_payload() {
            value.get_raw() >> (Self::tag_length(tag) as u32)
        } else {
            0
        };
        Self { tag, scalar }
    }
}

pub fn serialize(value: Value) -> StableValue {
    if value.is_ptr() {
        if value == STABLE_NULL_POINTER_32 {
            STABLE_NULL_POINTER
        } else {
            StableValue::from_ptr(value.get_ptr() as u64)
        }
    } else {
        let runtime_value = RuntimeValueEncoding::decode(value);
        let extended = if runtime_value.tag.is_signed() {
            runtime_value.scalar as i32 as i64 as u64
        } else {
            runtime_value.scalar as u64
        };
        let stable_value = StableValueEncoding {
            tag: runtime_value.tag,
            scalar: extended,
        };
        stable_value.encode()
    }
}

pub fn deserialize(value: StableValue) -> Value {
    if value.is_ptr() {
        if value == STABLE_NULL_POINTER {
            STABLE_NULL_POINTER_32
        } else {
            Value::from_ptr(checked_to_u32(value.get_ptr()) as usize)
        }
    } else {
        let stable_value = StableValueEncoding::decode(value);
        let narrowed = if stable_value.tag.is_signed() {
            checked_to_i32(stable_value.scalar as i64) as u32
        } else {
            checked_to_u32(stable_value.scalar)
        };
        let runtime_value = RuntimeValueEncoding {
            tag: stable_value.tag,
            scalar: narrowed,
        };
        runtime_value.encode()
    }
}
