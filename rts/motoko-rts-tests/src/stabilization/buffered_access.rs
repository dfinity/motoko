use std::{array::from_fn, cell::RefCell, mem::size_of};

use libc::{c_void, memcpy};
use motoko_rts::stabilization::buffered_access::{StableMemoryReaderWriter, PAGE_SIZE};
use oorandom::Rand32;

thread_local! {
    static STABLE_MEMORY: RefCell<Vec<u8>> = RefCell::new(vec![]);
}

#[no_mangle]
pub fn ic0_stable64_write(offset: u64, source: u64, size: u64) {
    STABLE_MEMORY.with(|memory| {
        assert!(offset + size <= memory.borrow().len() as u64);
        let destination = memory.borrow_mut().as_mut_ptr() as u64 + offset;
        unsafe {
            memcpy(
                destination as *mut c_void,
                source as *mut c_void,
                size as usize,
            );
        }
    });
}

#[no_mangle]
pub fn ic0_stable64_read(destination: u64, offset: u64, size: u64) {
    STABLE_MEMORY.with(|memory| {
        assert!(offset + size <= memory.borrow().len() as u64);
        let source = memory.borrow_mut().as_mut_ptr() as u64 + offset;
        unsafe {
            memcpy(
                destination as *mut c_void,
                source as *mut c_void,
                size as usize,
            );
        }
    });
}

#[no_mangle]
pub fn moc_stable_mem_size() -> u64 {
    STABLE_MEMORY.with(|memory| memory.borrow().len()) as u64 / PAGE_SIZE
}

#[no_mangle]
pub fn moc_stable_mem_grow(additional_pages: u64) -> u64 {
    for _ in 0..additional_pages * PAGE_SIZE {
        STABLE_MEMORY.with(|memory| memory.borrow_mut().push(0));
    }
    additional_pages
}

pub unsafe fn test() {
    println!("  Testing buffered access ...");
    test_empy_reader_writer();
    test_single_read_write();
    test_multiple_read_write();
    test_interleaved_read_write();
    test_bulk_read_write();
    test_randomized_read_write();
}

fn test_empy_reader_writer() {
    println!("    Testing empty reader writer ...");
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    let size = reader_writer.close();
    assert_eq!(size, 0);
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
    let size = reader_writer.close();
    assert_eq!(size, size_of::<u64>() as u64);
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
    let size = reader_writer.close();
    assert_eq!(size, (AMOUNT * size_of::<usize>()) as u64);
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
    let size = reader_writer.close();
    assert_eq!(size, (AMOUNT * size_of::<(usize, usize, usize)>()) as u64);
}

fn test_bulk_read_write() {
    println!("    Testing bulk read write ...");
    const LENGTH: usize = 99_999;
    let input: [u8; LENGTH] = from_fn(|index| (index % 256) as u8);
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    reader_writer.write(&input);
    assert!(!reader_writer.reading_finished());
    let mut output = [0u8; LENGTH];
    reader_writer.read(&mut output);
    assert_eq!(input, output);
    assert!(reader_writer.reading_finished());
    let size = reader_writer.close();
    assert_eq!(size, LENGTH as u64);
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
    let size = reader_writer.close();
    assert_eq!(size, total_size as u64);
}
