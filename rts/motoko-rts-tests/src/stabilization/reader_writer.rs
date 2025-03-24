use std::{array::from_fn, mem::size_of};

use motoko_rts::stabilization::serialization::stable_memory_stream::{
    ScanStream, StableMemoryStream, WriteStream,
};
use oorandom::Rand32;

use crate::stabilization::stable_memory::ic0_stable64_read;

pub unsafe fn test() {
    println!("  Testing stable memory stream ...");
    test_empy_reader_writer();
    test_single_read_write();
    test_single_update();
    test_multiple_read_write();
    test_multiple_updates();
    test_skip_all();
    test_interleaved_read_write();
    test_interleaved_skip();
    test_bulk_read_write();
    test_raw_read_write();
    test_randomized_read_write();
}

fn test_empy_reader_writer() {
    println!("    Testing empty stream ...");
    let mut reader_writer = StableMemoryStream::open(0);
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), 0);
}

fn test_single_read_write() {
    println!("    Testing single read write ...");
    let mut reader_writer = StableMemoryStream::open(0);
    const NUMBER: u64 = 1234567890;
    reader_writer.write(&NUMBER);
    let result = reader_writer.read::<u64>();
    assert_eq!(result, NUMBER);
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), size_of::<u64>() as u64);
}

fn test_single_update() {
    println!("    Testing single update ...");
    let mut reader_writer = StableMemoryStream::open(0);
    const NUMBER: u64 = 1234567890;
    reader_writer.write(&NUMBER);
    let result = reader_writer.read::<u64>();
    assert_eq!(result, NUMBER);
    assert!(reader_writer.scan_completed());
    const NEW_NUMBER: u64 = 321321321;
    reader_writer.update(&NEW_NUMBER);
    assert!(reader_writer.scan_completed());
    let mut test_value = 0u64;
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), size_of::<u64>() as u64);
    ic0_stable64_read(
        &mut test_value as *mut u64 as u64,
        0,
        size_of::<u64>() as u64,
    );
    assert_eq!(test_value, NEW_NUMBER);
}

fn test_multiple_read_write() {
    println!("    Testing multiple read write ...");
    let mut reader_writer = StableMemoryStream::open(0);
    const AMOUNT: usize = 100_000;
    for number in 0..AMOUNT {
        reader_writer.write(&number);
    }
    for number in 0..AMOUNT {
        assert!(!reader_writer.scan_completed());
        let output = reader_writer.read::<usize>();
        assert_eq!(output, number);
    }
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(
        reader_writer.written_length(),
        (AMOUNT * size_of::<usize>()) as u64
    );
}

fn test_multiple_updates() {
    println!("    Testing multiple read write ...");
    let mut reader_writer = StableMemoryStream::open(0);
    const AMOUNT: usize = 100_000;
    const TOTAL_LENGTH: u64 = (AMOUNT * size_of::<usize>()) as u64;
    for number in 0..AMOUNT {
        reader_writer.write(&number);
    }
    for number in 0..AMOUNT {
        assert!(!reader_writer.scan_completed());
        let output = reader_writer.read::<usize>();
        assert_eq!(output, number);
        reader_writer.update(&(number * 2));
    }
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), TOTAL_LENGTH);
    let mut test_data = [0usize; AMOUNT];
    ic0_stable64_read(&mut test_data[0] as *mut usize as u64, 0, TOTAL_LENGTH);
    for index in 0..AMOUNT {
        assert_eq!(test_data[index], index * 2)
    }
}

fn test_skip_all() {
    println!("    Testing skip all ...");
    let mut reader_writer = StableMemoryStream::open(0);
    const AMOUNT: usize = 100_000;
    let total_size = AMOUNT * size_of::<usize>();
    for number in 0..AMOUNT {
        reader_writer.write(&number);
    }
    assert!(!reader_writer.scan_completed());
    reader_writer.skip(total_size);
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), total_size as u64);
}

fn test_interleaved_read_write() {
    println!("    Testing interleaved read write ...");
    let mut reader_writer = StableMemoryStream::open(0);
    const AMOUNT: usize = 100_000;
    for counter in 0..AMOUNT {
        let input = (counter, counter * 2, counter * 3);
        reader_writer.write(&input);
        assert!(!reader_writer.scan_completed());
        let output = reader_writer.read::<(usize, usize, usize)>();
        assert_eq!(output, input);
        assert!(reader_writer.scan_completed());
    }
    reader_writer.close();
    assert_eq!(
        reader_writer.written_length(),
        (AMOUNT * size_of::<(usize, usize, usize)>()) as u64
    );
}

fn test_interleaved_skip() {
    println!("    Testing interleaved read skip ...");
    let mut reader_writer = StableMemoryStream::open(0);
    let value_size = size_of::<(usize, usize, usize)>();
    const AMOUNT: usize = 100_000;
    for counter in 0..AMOUNT {
        reader_writer.write(&(counter, counter * 2, counter * 3));
        assert!(!reader_writer.scan_completed());
    }
    for counter in 0..AMOUNT {
        if counter % 2 == 0 {
            reader_writer.skip(value_size)
        } else {
            let output = reader_writer.read::<(usize, usize, usize)>();
            assert_eq!(output, (counter, counter * 2, counter * 3));
        }
    }
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), (AMOUNT * value_size) as u64);
}

fn test_bulk_read_write() {
    println!("    Testing bulk read write ...");
    const LENGTH: usize = 99_999;
    let input: [u8; LENGTH] = from_fn(|index| index as u8);
    let mut reader_writer = StableMemoryStream::open(0);
    reader_writer.write(&input);
    assert!(!reader_writer.scan_completed());
    let mut output = [0u8; LENGTH];
    reader_writer.raw_read(&mut output as *mut u8 as usize, LENGTH);
    assert_eq!(input, output);
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), LENGTH as u64);
}

fn test_raw_read_write() {
    println!("    Testing raw read write ...");
    const LENGTH: usize = 99;
    let mut reader_writer = StableMemoryStream::open(0);
    const AMOUNT: usize = 100;
    for counter in 0..AMOUNT {
        let input: [u8; LENGTH] = from_fn(|index| (counter + index) as u8);
        reader_writer.raw_write(&input[0] as *const u8 as usize, LENGTH);
        assert!(!reader_writer.scan_completed());
    }
    for counter in 0..AMOUNT {
        let mut output = [0u8; LENGTH];
        reader_writer.raw_read(&mut output[0] as *mut u8 as usize, LENGTH);
        assert_eq!(from_fn(|index| (counter + index) as u8), output);
    }
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), (AMOUNT * LENGTH) as u64);
}

#[repr(C)]
#[derive(Debug, PartialEq, Default)]
struct RandomRecord {
    field_0: i8,
    padding_0: i8,
    field_1: i16,
    field_2: i32,
    field_4: i64,
    field_5: i128,
    field_6: f64,
    padding_6: i64,
}

type RandomTuple = (f64, i64);
type RandomArray = [i64; 100];
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
                padding_0: 0,
                field_1: random.rand_i32() as i16,
                field_2: random.rand_i32(),
                field_4: random.rand_i32() as i64,
                field_5: random.rand_i32() as i128,
                field_6: random.rand_float() as f64,
                padding_6: 0,
            }),
            5 => RandomValue::ArrayValue(from_fn(|_| random.rand_i32() as i64)),
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

    fn write(&self, reader_writer: &mut StableMemoryStream) {
        match self {
            RandomValue::SingleByte(value) => reader_writer.write(value),
            RandomValue::SimpleNumber(value) => reader_writer.write(value),
            RandomValue::LargeNumber(value) => reader_writer.write(value),
            RandomValue::TupleValue(value) => reader_writer.write(value),
            RandomValue::RecordValue(value) => reader_writer.write(value),
            RandomValue::ArrayValue(value) => reader_writer.write(value),
            RandomValue::BulkValue(value) => reader_writer.write(value),
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
                padding_0: 0,
                field_1: 0,
                field_2: 0,
                field_4: 0,
                field_5: 0,
                field_6: 0.0,
                padding_6: 0,
            }),
            RandomValue::ArrayValue(_) => RandomValue::ArrayValue(from_fn(|_| 0)),
            RandomValue::BulkValue(_) => RandomValue::BulkValue(from_fn(|_| 0)),
        }
    }

    fn read(&mut self, reader_writer: &mut StableMemoryStream) {
        match self {
            RandomValue::SingleByte(value) => *value = reader_writer.read(),
            RandomValue::SimpleNumber(value) => *value = reader_writer.read(),
            RandomValue::LargeNumber(value) => *value = reader_writer.read(),
            RandomValue::TupleValue(value) => *value = reader_writer.read(),
            RandomValue::RecordValue(value) => *value = reader_writer.read(),
            RandomValue::ArrayValue(value) => {
                reader_writer.raw_read(value as *mut i64 as usize, size_of::<RandomArray>())
            }
            RandomValue::BulkValue(value) => {
                reader_writer.raw_read(value as *mut u8 as usize, size_of::<RandomBulkData>())
            }
        }
    }

    fn update(&self, reader_writer: &mut StableMemoryStream) {
        match self {
            RandomValue::SingleByte(value) => reader_writer.update(value),
            RandomValue::SimpleNumber(value) => reader_writer.update(value),
            RandomValue::LargeNumber(value) => reader_writer.update(value),
            RandomValue::TupleValue(value) => reader_writer.update(value),
            RandomValue::RecordValue(value) => reader_writer.update(value),
            RandomValue::ArrayValue(value) => reader_writer.update(value),
            RandomValue::BulkValue(value) => reader_writer.update(value),
        }
    }
}

fn test_randomized_read_write() {
    println!("    Testing randomized read write ...");
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    let mut series = vec![];
    let stable_start = random.rand_range(0..1000) as u64;
    let mut reader_writer = StableMemoryStream::open(stable_start);
    let mut total_size = 0;
    const AMOUNT: usize = 1000;
    for _ in 0..AMOUNT {
        let input = RandomValue::generate(&mut random);
        input.write(&mut reader_writer);
        total_size += input.size();
        series.push(input);
        assert!(!reader_writer.scan_completed());
        if random.rand_u32() % 2 == 0 {
            let expected = series.remove(0);
            let mut output = expected.empty_clone();
            output.read(&mut reader_writer);
            assert_eq!(output, expected);
            let empty = output.empty_clone();
            empty.update(&mut reader_writer);
        }
    }
    while !reader_writer.scan_completed() {
        let expected = series.remove(0);
        let mut output = expected.empty_clone();
        output.read(&mut reader_writer);
        assert_eq!(output, expected);
        let empty = output.empty_clone();
        empty.update(&mut reader_writer);
    }
    assert!(reader_writer.scan_completed());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), total_size as u64);
    check_zeroed_stable_memory(stable_start, total_size);
}

fn check_zeroed_stable_memory(stable_start: u64, size: usize) {
    for index in 0..size {
        let mut data = 0u8;
        ic0_stable64_read(&mut data as *mut u8 as u64, stable_start + index as u64, 1);
        assert_eq!(data, 0);
    }
}
