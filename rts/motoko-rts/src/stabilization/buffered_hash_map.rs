//! Hash map buffered in stable memory.
//! Used for address translation between main memory and stable memory or vice versa,
//! as needed for the serialization and deserialization.
//!
//! Dynamic growing of the hash table on defined occupation threshold.
//! Hash collisions are handled by inlined linked lists.
//!
//! Note: Use a common stable buffered memory for this hash map and stabilization/
//! destabilization. Otherwise, there may be stable memory inconsistencies due to
//! cache incoherences (e.g. changes not visible because they are cached in the
//! other memory, or unwanted overwrites if same page is independently modified in
//! different caches).

use core::{
    marker::PhantomData,
    mem::{size_of, zeroed},
    ops::Range,
    usize,
};

use super::buffered_stable_memory::BufferedStableMemory;

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
    /// Create a new hash map based on the buffered stable memory.
    pub fn new(buffer: &mut BufferedStableMemory) -> BufferedHashMap<K, V> {
        let mut hash_map = BufferedHashMap {
            table_length: INITIAL_TABLE_LENGTH,
            stored_entries: 0,
            key_type: PhantomData,
            value_type: PhantomData,
        };
        hash_map.initialize(buffer, 0..INITIAL_TABLE_LENGTH);
        hash_map
    }

    fn initialize(&mut self, buffer: &mut BufferedStableMemory, range: Range<usize>) {
        let empty = TableElement::empty();
        for index in range {
            self.write_element(buffer, index, &empty);
        }
    }

    /// Add a new key-value pair. The key must not yet exist.
    pub fn add(&mut self, buffer: &mut BufferedStableMemory, key: K, value: V) {
        debug_assert!(key != K::nil());
        if self.stored_entries >= self.table_length / 100 * OCCUPATION_THRESHOLD_PERCENT {
            self.grow(buffer);
        }
        let index = self.hash(key);
        let element = self.read_element(buffer, index);
        debug_assert!(element.entry.key != key); // Key must not yet exist.
        let next_collision = if element.entry.key != K::nil() {
            self.move_collision(buffer, index)
        } else {
            NO_COLLISION
        };
        self.write_element(
            buffer,
            index,
            &TableElement {
                entry: HashEntry { key, value },
                next_collision,
            },
        );
        self.stored_entries += 1;
    }

    fn grow(&mut self, buffer: &mut BufferedStableMemory) {
        let old_length = self.table_length;
        let new_length = self.table_length * GROWTH_FACTOR;
        // Backup all existing elements in additional space.
        let backup_start = self.element_offset(new_length);
        let mut backup_end = backup_start;
        for index in 0..old_length {
            let element = self.read_element(buffer, index);
            if element.entry.key != K::nil() {
                buffer.write(backup_end, &element.entry);
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
        self.initialize(buffer, 0..new_length);
        self.stored_entries = 0;
        // Re-insert and rehash all backed up elements.
        let mut read_offset = backup_start;
        while read_offset < backup_end {
            let entry = buffer.read::<HashEntry<K, V>>(read_offset);
            self.add(buffer, entry.key, entry.value);
            read_offset += size_of::<HashEntry<K, V>>() as u64;
        }
        debug_assert_eq!(
            (self.stored_entries * size_of::<HashEntry<K, V>>()) as u64,
            backup_end - backup_start
        );
        let backup_length = backup_end - backup_start;
        buffer.clear(backup_start, backup_length);
    }

    fn move_collision(&mut self, buffer: &mut BufferedStableMemory, index: usize) -> usize {
        let mut free = (index + 1) % self.table_length;
        while free != index && self.read_key(buffer, free) != K::nil() {
            free = (free + 1) % self.table_length;
        }
        debug_assert!(self.read_key(buffer, free) == K::nil()); // Free element must exist in a non-full table.
        let element = self.read_element(buffer, index);
        self.write_element(buffer, free, &element);
        free
    }

    /// Retrieve the value for a key, if the key exists. Returns `None` if the key is absent.
    pub fn get(&self, buffer: &mut BufferedStableMemory, key: K) -> Option<V> {
        debug_assert!(key != K::nil());
        let index = self.hash(key);
        let mut element = self.read_element(buffer, index);
        while element.entry.key != key && element.next_collision != NO_COLLISION {
            element = self.read_element(buffer, element.next_collision);
        }
        if element.entry.key == key {
            Some(element.entry.value)
        } else {
            None
        }
    }

    pub fn contains(&mut self, buffer: &mut BufferedStableMemory, key: K) -> bool {
        self.get(buffer, key).is_some()
    }

    fn read_key(&self, buffer: &mut BufferedStableMemory, index: usize) -> K {
        self.read_element(buffer, index).entry.key
    }

    fn read_element(&self, buffer: &mut BufferedStableMemory, index: usize) -> TableElement<K, V> {
        debug_assert!(index < self.table_length);
        let offset = self.element_offset(index);
        buffer.read(offset)
    }

    fn write_element(
        &mut self,
        buffer: &mut BufferedStableMemory,
        index: usize,
        element: &TableElement<K, V>,
    ) {
        debug_assert!(index < self.table_length);
        let offset = self.element_offset(index);
        buffer.write(offset, element);
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

// TODO: Remove these implementations, only used for RTS testing.
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
