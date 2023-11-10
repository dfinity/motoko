use std::{array::from_fn, mem::size_of};

use motoko_rts::stabilization::buffered_access::StableMemoryReaderWriter;
use oorandom::Rand32;

pub unsafe fn test() {
    println!("  Testing buffered access ...");
    test_empy_reader_writer();
    test_single_read_write();
    test_multiple_read_write();
    test_skip_all();
    test_interleaved_read_write();
    test_interleaved_skip();
    test_bulk_read_write();
    test_raw_read_write();
    test_randomized_read_write();
}

fn test_empy_reader_writer() {
    println!("    Testing empty reader writer ...");
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    assert!(reader_writer.reading_finished());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), 0);
}

fn test_single_read_write() {
    println!("    Testing single read write ...");
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    const NUMBER: u64 = 1234567890;
    reader_writer.write(&NUMBER);
    let mut result = 0u64;
    reader_writer.read(&mut result);
    assert_eq!(result, NUMBER);
    assert!(reader_writer.reading_finished());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), size_of::<u64>() as u64);
}

fn test_multiple_read_write() {
    println!("    Testing multiple read write ...");
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    const AMOUNT: usize = 100_000;
    for number in 0..AMOUNT {
        reader_writer.write(&number);
    }
    for number in 0..AMOUNT {
        assert!(!reader_writer.reading_finished());
        let mut output = 0usize;
        reader_writer.read(&mut output);
        assert_eq!(output, number);
    }
    assert!(reader_writer.reading_finished());
    reader_writer.close();
    assert_eq!(
        reader_writer.written_length(),
        (AMOUNT * size_of::<usize>()) as u64
    );
}

fn test_skip_all() {
    println!("    Testing skip all ...");
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    const AMOUNT: usize = 100_000;
    let total_size = AMOUNT * size_of::<usize>();
    for number in 0..AMOUNT {
        reader_writer.write(&number);
    }
    assert!(!reader_writer.reading_finished());
    reader_writer.skip(total_size);
    assert!(reader_writer.reading_finished());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), total_size as u64);
}

fn test_interleaved_read_write() {
    println!("    Testing interleaved read write ...");
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    const AMOUNT: usize = 100_000;
    for counter in 0..AMOUNT {
        let input = (counter, counter * 2, counter * 3);
        reader_writer.write(&input);
        assert!(!reader_writer.reading_finished());
        let mut output = (0usize, 0usize, 0usize);
        reader_writer.read(&mut output);
        assert_eq!(output, input);
        assert!(reader_writer.reading_finished());
    }
    reader_writer.close();
    assert_eq!(
        reader_writer.written_length(),
        (AMOUNT * size_of::<(usize, usize, usize)>()) as u64
    );
}

fn test_interleaved_skip() {
    println!("    Testing interleaved read skip ...");
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    let value_size = size_of::<(usize, usize, usize)>();
    const AMOUNT: usize = 100_000;
    for counter in 0..AMOUNT {
        reader_writer.write(&(counter, counter * 2, counter * 3));
        assert!(!reader_writer.reading_finished());
    }
    for counter in 0..AMOUNT {
        if counter % 2 == 0 {
            reader_writer.skip(value_size)
        } else {
            let mut output = (0usize, 0usize, 0usize);
            reader_writer.read(&mut output);
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
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    reader_writer.write(&input);
    assert!(!reader_writer.reading_finished());
    let mut output = [0u8; LENGTH];
    reader_writer.read(&mut output);
    assert_eq!(input, output);
    assert!(reader_writer.reading_finished());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), LENGTH as u64);
}

fn test_raw_read_write() {
    println!("    Testing raw read write ...");
    const LENGTH: usize = 99;
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    const AMOUNT: usize = 100;
    for counter in 0..AMOUNT {
        let input: [u8; LENGTH] = from_fn(|index| (counter + index) as u8);
        reader_writer.raw_write(&input[0] as *const u8 as usize, LENGTH);
        assert!(!reader_writer.reading_finished());
    }
    for counter in 0..AMOUNT {
        let mut output = [0u8; LENGTH];
        reader_writer.raw_read(&mut output[0] as *mut u8 as usize, LENGTH);
        assert_eq!(from_fn(|index| (counter + index) as u8), output);
    }
    assert!(reader_writer.reading_finished());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), (AMOUNT * LENGTH) as u64);
}

#[derive(Debug, PartialEq)]
struct RandomRecord {
    field_0: i8,
    field_1: i16,
    field_2: i32,
    field_4: i64,
    field_5: i128,
    field_6: f64,
}

type RandomTuple = (f32, i64);
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
                random.rand_float(),
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

    fn write(&self, reader_writer: &mut StableMemoryReaderWriter) {
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

    fn read(&mut self, reader_writer: &mut StableMemoryReaderWriter) {
        match self {
            RandomValue::SingleByte(value) => reader_writer.read(value),
            RandomValue::SimpleNumber(value) => reader_writer.read(value),
            RandomValue::LargeNumber(value) => reader_writer.read(value),
            RandomValue::TupleValue(value) => reader_writer.read(value),
            RandomValue::RecordValue(value) => reader_writer.read(value),
            RandomValue::ArrayValue(value) => reader_writer.read(value),
            RandomValue::BulkValue(value) => reader_writer.read(value),
        }
    }
}

fn test_randomized_read_write() {
    println!("    Testing randomized read write ...");
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    let mut series = vec![];
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    let mut total_size = 0;
    const AMOUNT: usize = 1000;
    for _ in 0..AMOUNT {
        let input = RandomValue::generate(&mut random);
        input.write(&mut reader_writer);
        total_size += input.size();
        series.push(input);
        assert!(!reader_writer.reading_finished());
        if random.rand_u32() % 2 == 0 {
            let expected = series.remove(0);
            let mut output = expected.empty_clone();
            output.read(&mut reader_writer);
            assert_eq!(output, expected);
        }
    }
    while !reader_writer.reading_finished() {
        let expected = series.remove(0);
        let mut output = expected.empty_clone();
        output.read(&mut reader_writer);
        assert_eq!(output, expected);
    }
    assert!(reader_writer.reading_finished());
    reader_writer.close();
    assert_eq!(reader_writer.written_length(), total_size as u64);
}
