use motoko_rts::stabilization::layout::value_tagging::{
    deserialize, serialize, RuntimeValueEncoding, StableValueEncoding, ValueTag,
};

pub unsafe fn test() {
    println!("  Testing value tagging ...");

    test_constants();
    test_bigints();
    test_int64s();
    test_nat64s();
    test_int32s();
    test_nat32s();
    test_chars();
    test_int16s();
    test_nat16s();
    test_int8s();
    test_nat8s();
}

fn test_constants() {
    test_tagging(ValueTag::FALSE, 0, 0b0);
    test_tagging(ValueTag::TRUE, 0, 0b1);
    test_tagging(ValueTag::UNIT, 0, 0b01000000_00000000_00000000_00000000);
}

fn test_bigints() {
    let tag = ValueTag::BIGINT;
    test_tagging(tag, 0, 0b10);
    test_tagging(tag, 1, 0b110);
    test_tagging(tag, -1i32 as u32, 0b11111111_11111111_11111111_11111110);
    test_tagging(tag, 123, 494);
    test_tagging(tag, -123i32 as u32, -490i32 as u32);
    test_tagging(tag, (1 << 29) - 1, 0b01111111_11111111_11111111_11111110);
    test_tagging(
        tag,
        -(1i32 << 29) as u32,
        0b10000000_00000000_00000000_00000010,
    );
}

fn test_int64s() {
    let tag = ValueTag::INT64;
    test_tagging(tag, 0, 0b1100);
    test_tagging(tag, 1, 0b11100);
    test_tagging(tag, -1i32 as u32, 0b11111111_11111111_11111111_11111100);
    test_tagging(tag, 123, 1980);
    test_tagging(tag, -123i32 as u32, -1956i32 as u32);
    test_tagging(tag, (1 << 27) - 1, 0b01111111_11111111_11111111_11111100);
    test_tagging(
        tag,
        -(1i32 << 27) as u32,
        0b10000000_00000000_00000000_00001100,
    );
}

fn test_nat64s() {
    let tag = ValueTag::NAT64;
    test_tagging(tag, 0, 0b0100);
    test_tagging(tag, 1, 0b10100);
    test_tagging(tag, 123, 1972);
    test_tagging(tag, (1 << 28) - 1, 0b11111111_11111111_11111111_11110100);
}

fn test_int32s() {
    let tag = ValueTag::INT32;
    test_tagging(tag, 0, 0b11000);
    test_tagging(tag, 1, 0b111000);
    test_tagging(tag, -1i32 as u32, 0b11111111_11111111_11111111_11111000);
    test_tagging(tag, 123, 3960);
    test_tagging(tag, -123i32 as u32, -3912i32 as u32);
    test_tagging(tag, (1 << 26) - 1, 0b01111111_11111111_11111111_11111000);
    test_tagging(
        tag,
        -(1i32 << 26) as u32,
        0b10000000_00000000_00000000_00011000,
    );
}

fn test_nat32s() {
    let tag = ValueTag::NAT32;
    test_tagging(tag, 0, 0b01000);
    test_tagging(tag, 1, 0b101000);
    test_tagging(tag, 123, 3944);
    test_tagging(tag, (1 << 27) - 1, 0b11111111_11111111_11111111_11101000);
}

fn test_chars() {
    let tag = ValueTag::CHAR;
    test_tagging(tag, 0, 0b010_00000000);
    test_tagging(tag, 1, 0b1010_00000000);
    test_tagging(tag, 123, 252416);
    test_tagging(tag, (1 << 21) - 1, 0b11111111_11111111_11111010_00000000);
}

fn test_int16s() {
    let tag = ValueTag::INT16;
    test_tagging(tag, 0, 0b11000000_00000000);
    test_tagging(tag, 1, 0b1_11000000_00000000);
    test_tagging(tag, -1i32 as u32, 0b11111111_11111111_11000000_00000000);
    test_tagging(tag, 123, 8110080);
    test_tagging(tag, -123i32 as u32, -8011776i32 as u32);
    test_tagging(tag, (1 << 15) - 1, 0b01111111_11111111_11000000_00000000);
    test_tagging(
        tag,
        -(1i32 << 15) as u32,
        0b10000000_00000000_11000000_00000000,
    );
}

fn test_nat16s() {
    let tag = ValueTag::NAT16;
    test_tagging(tag, 0, 0b01000000_00000000);
    test_tagging(tag, 1, 0b1_01000000_00000000);
    test_tagging(tag, 123, 8077312);
    test_tagging(tag, (1 << 16) - 1, 0b11111111_11111111_01000000_00000000);
}

fn test_int8s() {
    let tag = ValueTag::INT8;
    test_tagging(tag, 0, 0b11000000_00000000_00000000);
    test_tagging(tag, 1, 0b1_11000000_00000000_00000000);
    test_tagging(tag, -1i32 as u32, 0b11111111_11000000_00000000_00000000);
    test_tagging(tag, 123, 2076180480);
    test_tagging(tag, -123i32 as u32, -2051014656i32 as u32);
    test_tagging(tag, (1 << 7) - 1, 0b01111111_11000000_00000000_00000000);
    test_tagging(
        tag,
        -(1i32 << 7) as u32,
        0b10000000_11000000_00000000_00000000,
    );
}

fn test_nat8s() {
    let tag = ValueTag::NAT8;
    test_tagging(tag, 0, 0b01000000_00000000_00000000);
    test_tagging(tag, 1, 0b1_01000000_00000000_00000000);
    test_tagging(tag, 123, 2067791872);
    test_tagging(tag, (1 << 8) - 1, 0b11111111_01000000_00000000_00000000);
}

fn test_tagging(tag: ValueTag, scalar: u32, tagged_value: u32) {
    let runtime_encoding = RuntimeValueEncoding { tag, scalar };
    let runtime_value = runtime_encoding.encode();
    assert_eq!(runtime_value.get_raw(), tagged_value);
    let stable_value = serialize(runtime_value);
    let stable_encoding = StableValueEncoding::decode(stable_value);
    assert_eq!(stable_encoding.tag, tag);
    if tag.is_signed() {
        assert_eq!(stable_encoding.scalar, scalar as i32 as i64 as u64);
    } else {
        assert_eq!(stable_encoding.scalar, scalar as u64);
    }
    let deserialized = deserialize(stable_value);
    assert_eq!(deserialized, runtime_value);
}
