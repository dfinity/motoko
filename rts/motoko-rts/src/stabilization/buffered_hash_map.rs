//! Hash map buffered in stable memory.
//! Used for address translation between main memory and stable memory or vice versa,
//! as needed for the serialization and deserialization.
//!
//! Dynamic growing of the hash table on defined occupation threshold.
//! Hash collisions are handled by inlined linked lists.
//!
//! Note: The underlying buffered stable memory must be page-wise disjoint to other
//! buffered stable memory used for the object serialization or deserialization.

use core::{
    marker::PhantomData,
    mem::{size_of, zeroed},
    ops::Range,
    usize,
};

use crate::memory::Memory;

use super::buffered_stable_memory::{BufferedStableMemory, MAXIMUM_CACHED_PAGES};

const NO_COLLISION: usize = usize::MAX;

pub trait Hashable: PartialEq + Copy {
    fn nil() -> Self;
    fn hash(&self) -> usize;
}

#[repr(C)]
struct HashEntry<K: Hashable, V> {
    key: K,
    value: V,
}

#[repr(C)]
struct TableElement<K: Hashable, V> {
    entry: HashEntry<K, V>,
    /// Index of next element containing collision.
    next_collision: usize,
}

impl<K: Hashable, V> TableElement<K, V> {
    fn empty() -> TableElement<K, V> {
        TableElement {
            entry: HashEntry {
                key: K::nil(),
                value: unsafe { zeroed() },
            },
            next_collision: NO_COLLISION,
        }
    }
}

pub struct BufferedHashMap<K: Hashable, V> {
    buffer: BufferedStableMemory,
    table_length: usize,
    stored_entries: usize,
    // Avoids compiler on `K` and `V` because they are not used in field declaration.
    // Instead, `K` and `V` are used in low-level stable memory accesses.
    key_type: PhantomData<K>,
    value_type: PhantomData<V>,
}

const INITIAL_TABLE_LENGTH: usize = 1024;
const GROWTH_FACTOR: usize = 2;
const OCCUPATION_THRESHOLD_PERCENT: usize = 65;

impl<K: Hashable, V> BufferedHashMap<K, V> {
    pub fn new<M: Memory>(mem: &mut M, stable_base_address: u64) -> BufferedHashMap<K, V> {
        let buffer = BufferedStableMemory::open(mem, MAXIMUM_CACHED_PAGES, stable_base_address);
        let mut hash_map = BufferedHashMap {
            buffer,
            table_length: INITIAL_TABLE_LENGTH,
            stored_entries: 0,
            key_type: PhantomData,
            value_type: PhantomData,
        };
        hash_map.initialize(0..INITIAL_TABLE_LENGTH);
        hash_map
    }

    pub fn free(&mut self) {
        let table_size = self.element_offset(self.table_length);
        self.buffer.clear(0, table_size);
        self.buffer.close();
    }

    fn initialize(&mut self, range: Range<usize>) {
        let empty = TableElement::empty();
        for index in range {
            self.write_element(index, &empty);
        }
    }

    pub fn add(&mut self, key: K, value: V) {
        debug_assert!(key != K::nil());
        if self.stored_entries >= self.table_length / 100 * OCCUPATION_THRESHOLD_PERCENT {
            self.grow();
        }
        let index = self.hash(key);
        let element = self.read_element(index);
        debug_assert!(element.entry.key != key); // Key must not yet exist.
        let next_collision = if element.entry.key != K::nil() {
            self.move_collision(index)
        } else {
            NO_COLLISION
        };
        self.write_element(
            index,
            &TableElement {
                entry: HashEntry { key, value },
                next_collision,
            },
        );
        self.stored_entries += 1;
    }

    fn grow(&mut self) {
        let old_length = self.table_length;
        let new_length = self.table_length * GROWTH_FACTOR;
        // Backup all existing elements in additional space.
        let backup_start = self.element_offset(new_length);
        let mut backup_end = backup_start;
        for index in 0..old_length {
            let element = self.read_element(index);
            if element.entry.key != K::nil() {
                self.buffer.write(backup_end, &element.entry);
                backup_end += size_of::<HashEntry<K, V>>() as u64;
            } else {
                debug_assert_eq!(element.next_collision, NO_COLLISION);
            }
        }
        debug_assert_eq!(
            (self.stored_entries * size_of::<HashEntry<K, V>>()) as u64,
            backup_end - backup_start
        );
        // Initialize new larger hash table.
        self.table_length = new_length;
        self.initialize(0..new_length);
        self.stored_entries = 0;
        // Re-insert and rehash all backed up elements.
        let mut read_offset = backup_start;
        while read_offset < backup_end {
            let entry = self.buffer.read::<HashEntry<K, V>>(read_offset);
            self.add(entry.key, entry.value);
            read_offset += size_of::<HashEntry<K, V>>() as u64;
        }
        debug_assert_eq!(
            (self.stored_entries * size_of::<HashEntry<K, V>>()) as u64,
            backup_end - backup_start
        );
        let backup_length = backup_end - backup_start;
        self.buffer.clear(backup_start, backup_length);
    }

    fn move_collision(&mut self, index: usize) -> usize {
        let mut free = (index + 1) % self.table_length;
        while free != index && self.read_key(free) != K::nil() {
            free = (free + 1) % self.table_length;
        }
        debug_assert!(self.read_key(free) == K::nil()); // Free element must exist in a non-full table.
        let element = self.read_element(index);
        self.write_element(free, &element);
        free
    }

    pub fn get(&mut self, key: K) -> V {
        debug_assert!(key != K::nil());
        let index = self.hash(key);
        let mut element = self.read_element(index);
        while element.entry.key != key && element.next_collision != NO_COLLISION {
            element = self.read_element(element.next_collision);
        }
        assert!(element.entry.key == key); // Key must exist.
        element.entry.value
    }

    fn read_key(&mut self, index: usize) -> K {
        self.read_element(index).entry.key
    }

    fn read_element(&mut self, index: usize) -> TableElement<K, V> {
        debug_assert!(index < self.table_length);
        let offset = self.element_offset(index);
        self.buffer.read(offset)
    }

    fn write_element(&mut self, index: usize, element: &TableElement<K, V>) {
        debug_assert!(index < self.table_length);
        let offset = self.element_offset(index);
        self.buffer.write(offset, element);
    }

    fn element_offset(&self, index: usize) -> u64 {
        // May also be used to determine table end position or position in extended table.
        (index * size_of::<TableElement<K, V>>()) as u64
    }

    fn hash(&self, key: K) -> usize {
        debug_assert!(key != K::nil());
        key.hash() % self.table_length as usize
    }
}

impl Hashable for usize {
    fn nil() -> Self {
        usize::MAX
    }

    fn hash(&self) -> usize {
        *self
    }
}

impl Hashable for u64 {
    fn nil() -> Self {
        u64::MAX
    }

    fn hash(&self) -> usize {
        *self as usize
    }
}
