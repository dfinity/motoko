use std::{array::from_fn, mem::size_of};

use motoko_rts::{
    constants::WORD_SIZE,
    stabilization::buffered_stable_memory::BufferedStableMemory,
    types::{Blob, Words},
};
use oorandom::Rand32;

use crate::{memory::TestMemory, stabilization::stable_memory::ic0_stable64_read};

const BUFFER_SIZE: Words<u32> = Words((4 * 1024 * 1024 + size_of::<Blob>() as u32) / WORD_SIZE);

pub unsafe fn test() {
    println!("  Testing buffered stable memory ...");
    test_empy_reader_writer();
    test_single_read_write();
    test_multiple_read_write();
    test_interleaved_read_write();
    test_bulk_read_write();
    test_raw_read_write();
    test_randomized_read_write();
}

fn test_empy_reader_writer() {
    println!("    Testing empty buffer ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, 0);
    buffer.close();
}

fn test_single_read_write() {
    println!("    Testing single read write ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, 0);
    const NUMBER: u64 = 1234567890;
    const OFFSET: u64 = 7;
    buffer.write(OFFSET, &NUMBER);
    let result = buffer.read::<u64>(OFFSET);
    assert_eq!(result, NUMBER);
    buffer.close();
    memory = TestMemory::new(BUFFER_SIZE);
    buffer = BufferedStableMemory::open(&mut memory, 0);
    let result = buffer.read::<u64>(OFFSET);
    assert_eq!(result, NUMBER);
    buffer.close();
}

fn test_multiple_read_write() {
    println!("    Testing multiple read write ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, 0);
    const AMOUNT: usize = 100_000;
    let mut offset = 0u64;
    for number in 0..AMOUNT {
        buffer.write(offset, &number);
        offset += size_of::<usize>() as u64;
    }
    offset = 0;
    for number in 0..AMOUNT {
        let output = buffer.read::<usize>(offset);
        assert_eq!(output, number);
        offset += size_of::<usize>() as u64;
    }
    buffer.close();
    memory = TestMemory::new(BUFFER_SIZE);
    buffer = BufferedStableMemory::open(&mut memory, 0);
    offset = 0;
    for number in 0..AMOUNT {
        let output = buffer.read::<usize>(offset);
        assert_eq!(output, number);
        offset += size_of::<usize>() as u64;
    }
    buffer.close();
}

fn test_interleaved_read_write() {
    println!("    Testing interleaved read write ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, 0);
    const AMOUNT: usize = 100_000;
    let mut offset = 0u64;
    for counter in 0..AMOUNT {
        let input = (counter, counter * 2, counter * 3);
        buffer.write(offset, &input);
        let output = buffer.read::<(usize, usize, usize)>(offset);
        assert_eq!(output, input);
        offset += size_of::<(usize, usize, usize)>() as u64;
    }
    buffer.close();
    memory = TestMemory::new(BUFFER_SIZE);
    buffer = BufferedStableMemory::open(&mut memory, 0);
    offset = 0;
    for counter in 0..AMOUNT {
        let input = (counter, counter * 2, counter * 3);
        let output = buffer.read::<(usize, usize, usize)>(offset);
        assert_eq!(output, input);
        offset += size_of::<(usize, usize, usize)>() as u64;
    }
    buffer.close();
}

fn test_bulk_read_write() {
    println!("    Testing bulk read write ...");
    const LENGTH: usize = 99_999;
    let input: [u8; LENGTH] = from_fn(|index| index as u8);
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, 0);
    const OFFSET: u64 = 123;
    buffer.write(OFFSET, &input);
    let mut output = [0u8; LENGTH];
    buffer.raw_read(OFFSET, &mut output as *mut u8 as usize, LENGTH);
    assert_eq!(input, output);
    buffer.close();
    memory = TestMemory::new(BUFFER_SIZE);
    buffer = BufferedStableMemory::open(&mut memory, 0);
    let mut output = [0u8; LENGTH];
    buffer.raw_read(OFFSET, &mut output as *mut u8 as usize, LENGTH);
    assert_eq!(input, output);
    buffer.close();
}

fn test_raw_read_write() {
    println!("    Testing raw read write ...");
    const LENGTH: usize = 99;
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, 0);
    const AMOUNT: usize = 100;
    let mut offset = 0u64;
    for counter in 0..AMOUNT {
        let input: [u8; LENGTH] = from_fn(|index| (counter + index) as u8);
        buffer.raw_write(offset, &input[0] as *const u8 as usize, LENGTH);
        offset += LENGTH as u64;
    }
    offset = 0;
    for counter in 0..AMOUNT {
        let mut output = [0u8; LENGTH];
        buffer.raw_read(offset, &mut output[0] as *mut u8 as usize, LENGTH);
        assert_eq!(from_fn(|index| (counter + index) as u8), output);
        offset += LENGTH as u64;
    }
    buffer.close();
    memory = TestMemory::new(BUFFER_SIZE);
    buffer = BufferedStableMemory::open(&mut memory, 0);
    offset = 0;
    for counter in 0..AMOUNT {
        let mut output = [0u8; LENGTH];
        buffer.raw_read(offset, &mut output[0] as *mut u8 as usize, LENGTH);
        assert_eq!(from_fn(|index| (counter + index) as u8), output);
        offset += LENGTH as u64;
    }
    buffer.close();
}

#[derive(Debug, PartialEq, Default)]
struct RandomRecord {
    field_0: i8,
    field_1: i16,
    field_2: i32,
    field_4: i64,
    field_5: i128,
    field_6: f64,
}

type RandomTuple = (f64, i64);
type RandomArray = [i32; 100];
type RandomBulkData = [u8; 10_000];

#[derive(Debug, PartialEq)]
enum RandomValue {
    SingleByte(u8),
    SimpleNumber(usize),
    LargeNumber(u128),
    TupleValue(RandomTuple),
    RecordValue(RandomRecord),
    ArrayValue(RandomArray),
    BulkValue(RandomBulkData),
}

impl RandomValue {
    fn generate(random: &mut Rand32) -> RandomValue {
        match random.rand_u32() % 7 {
            0 => RandomValue::SingleByte(random.rand_u32() as u8),
            1 => RandomValue::SimpleNumber(random.rand_u32() as usize),
            2 => RandomValue::LargeNumber(
                random.rand_u32() as u128
                    * random.rand_u32() as u128
                    * random.rand_u32() as u128
                    * random.rand_u32() as u128,
            ),
            3 => RandomValue::TupleValue((
                random.rand_float() as f64,
                random.rand_i32() as i64 * random.rand_i32() as i64,
            )),
            4 => RandomValue::RecordValue(RandomRecord {
                field_0: random.rand_i32() as i8,
                field_1: random.rand_i32() as i16,
                field_2: random.rand_i32(),
                field_4: random.rand_i32() as i64,
                field_5: random.rand_i32() as i128,
                field_6: random.rand_float() as f64,
            }),
            5 => RandomValue::ArrayValue(from_fn(|_| random.rand_i32())),
            6 => RandomValue::BulkValue(from_fn(|_| random.rand_u32() as u8)),
            _ => unreachable!(),
        }
    }

    fn size(&self) -> usize {
        match self {
            RandomValue::SingleByte(_) => size_of::<u8>(),
            RandomValue::SimpleNumber(_) => size_of::<usize>(),
            RandomValue::LargeNumber(_) => size_of::<u128>(),
            RandomValue::TupleValue(_) => size_of::<RandomTuple>(),
            RandomValue::RecordValue(_) => size_of::<RandomRecord>(),
            RandomValue::ArrayValue(_) => size_of::<RandomArray>(),
            RandomValue::BulkValue(_) => size_of::<RandomBulkData>(),
        }
    }

    fn write(&self, buffer: &mut BufferedStableMemory, offset: u64) {
        match self {
            RandomValue::SingleByte(value) => buffer.write(offset, value),
            RandomValue::SimpleNumber(value) => buffer.write(offset, value),
            RandomValue::LargeNumber(value) => buffer.write(offset, value),
            RandomValue::TupleValue(value) => buffer.write(offset, value),
            RandomValue::RecordValue(value) => buffer.write(offset, value),
            RandomValue::ArrayValue(value) => buffer.write(offset, value),
            RandomValue::BulkValue(value) => buffer.write(offset, value),
        }
    }

    fn empty_clone(&self) -> RandomValue {
        match self {
            RandomValue::SingleByte(_) => RandomValue::SingleByte(0),
            RandomValue::SimpleNumber(_) => RandomValue::SimpleNumber(0),
            RandomValue::LargeNumber(_) => RandomValue::LargeNumber(0),
            RandomValue::TupleValue(_) => RandomValue::TupleValue((0.0, 0)),
            RandomValue::RecordValue(_) => RandomValue::RecordValue(RandomRecord {
                field_0: 0,
                field_1: 0,
                field_2: 0,
                field_4: 0,
                field_5: 0,
                field_6: 0.0,
            }),
            RandomValue::ArrayValue(_) => RandomValue::ArrayValue(from_fn(|_| 0)),
            RandomValue::BulkValue(_) => RandomValue::BulkValue(from_fn(|_| 0)),
        }
    }

    fn read(&mut self, buffer: &mut BufferedStableMemory, offset: u64) {
        match self {
            RandomValue::SingleByte(value) => *value = buffer.read(offset),
            RandomValue::SimpleNumber(value) => *value = buffer.read(offset),
            RandomValue::LargeNumber(value) => *value = buffer.read(offset),
            RandomValue::TupleValue(value) => *value = buffer.read(offset),
            RandomValue::RecordValue(value) => *value = buffer.read(offset),
            RandomValue::ArrayValue(value) => {
                buffer.raw_read(offset, value as *mut i32 as usize, size_of::<RandomArray>())
            }
            RandomValue::BulkValue(value) => buffer.raw_read(
                offset,
                value as *mut u8 as usize,
                size_of::<RandomBulkData>(),
            ),
        }
    }
}

fn test_randomized_read_write() {
    println!("    Testing randomized read write ...");
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    let mut series = vec![];
    let stable_start = random.rand_range(0..1000) as u64;
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, stable_start);
    const AMOUNT: usize = 1000;
    let mut write_offset = 0u64;
    let mut read_offset = 0u64;
    for _ in 0..AMOUNT {
        let input = RandomValue::generate(&mut random);
        input.write(&mut buffer, write_offset);
        write_offset += input.size() as u64;
        series.push(input);
        if random.rand_u32() % 2 == 0 {
            let expected = series.remove(0);
            let mut output = expected.empty_clone();
            output.read(&mut buffer, read_offset);
            assert_eq!(output, expected);
            let empty = output.empty_clone();
            empty.write(&mut buffer, read_offset);
            if read_offset == 0 {
                buffer.write(4, &0usize);
                assert_eq!(buffer.read::<usize>(4), 0);
            }
            read_offset += output.size() as u64;
        }
    }
    while read_offset < write_offset {
        let expected = series.remove(0);
        let mut output = expected.empty_clone();
        output.read(&mut buffer, read_offset);
        assert_eq!(output, expected);
        let empty = output.empty_clone();
        empty.write(&mut buffer, read_offset);
        read_offset += output.size() as u64;
    }
    buffer.close();
    check_zeroed_stable_memory(stable_start, write_offset as usize);
}

fn check_zeroed_stable_memory(stable_start: u64, size: usize) {
    for index in 0..size {
        let mut data = 0u8;
        ic0_stable64_read(&mut data as *mut u8 as u64, stable_start + index as u64, 1);
        assert_eq!(data, 0);
    }
}
