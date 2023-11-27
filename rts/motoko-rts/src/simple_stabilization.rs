use crate::{
    memory::Memory,
    stabilization::{
        buffered_hash_map::{BufferedHashMap, Hashable},
        buffered_stable_memory::BufferedStableMemory,
        layout::StableValue,
        traversal_stack::TraversalStack,
    },
    types::{Array, Value, TAG_ARRAY, TAG_ARRAY_SLICE_MIN, TRUE_VALUE},
    visitor::visit_pointer_fields,
};

struct Serialization<'a, M: Memory> {
    mem: &'a mut M,
    buffer: BufferedStableMemory,
    stack: TraversalStack<Value>,
    map: BufferedHashMap<Value, StableValue>,
    free: u64,
}

impl Hashable for Value {
    fn nil() -> Self {
        Value::from_ptr(0)
    }

    fn hash(&self) -> usize {
        self.get_raw() as usize
    }
}

impl<'a, M: Memory> Serialization<'a, M> {
    pub fn new(mem: &'a mut M, stable_start: u64) -> Serialization<'a, M> {
        let mut buffer = BufferedStableMemory::open(mem, stable_start);
        let stack = TraversalStack::new(mem);
        let map = BufferedHashMap::new(&mut buffer);
        Serialization {
            mem,
            buffer,
            stack,
            map,
            free: 0,
        }
    }

    pub fn run(&mut self, stable_root: Value) {
        Self::clear_array_slicing(stable_root);
        self.stack.push(self.mem, stable_root);
        while !self.stack.is_empty() {
            let current = self.stack.pop();
            if !self.map.contains(&mut self.buffer, current) {
                let target = self.allocate(current);
                self.map.add(&mut self.buffer, current, target);
                // TODO: Handle array tag with slice information.
                unsafe {
                    // TODO: Abstract as simpler helper function
                    // Include visiting static objects, but exclude the `true` literal that is a skewed value.
                    const HEAP_BASE: usize = TRUE_VALUE as usize + 1;
                    visit_pointer_fields(
                        self,
                        current.as_obj(),
                        current.tag(),
                        HEAP_BASE,
                        |context, field_address| {
                            let field = (*field_address).forward_if_possible();
                            if field.is_ptr() && !context.map.contains(&mut context.buffer, field) {
                                Self::clear_array_slicing(field);
                                context.stack.push(context.mem, field);
                            }
                        },
                        |context, _, array| {
                            let length = Self::slice_array(array);
                            if (*array).header.tag >= TAG_ARRAY_SLICE_MIN {
                                context
                                    .stack
                                    .push(context.mem, Value::from_ptr(array as usize));
                            }
                            length
                        },
                    )
                }
            }
        }
    }

    fn allocate(&mut self, object: Value) -> StableValue {
        let size = self.stable_size(object);
        let address = self.free;
        self.free += size;
        StableValue::from_address(address)
    }

    fn stable_size(&self, object: Value) -> u64 {
        todo!()
    }

    // TODO: Unify with the incremental GC's array slice helper function.
    fn slice_array(array: *mut Array) -> u32 {
        const SLICE_LENGTH: u32 = 128;
        debug_assert!(SLICE_LENGTH >= TAG_ARRAY_SLICE_MIN);
        unsafe {
            let tag = (*array).header.tag;
            let slice_start = if tag >= TAG_ARRAY_SLICE_MIN { tag } else { 0 };
            if array.len() - slice_start > SLICE_LENGTH {
                let new_start = slice_start + SLICE_LENGTH;
                (*array).header.tag = new_start;
                new_start
            } else {
                (*array).header.tag = TAG_ARRAY;
                array.len()
            }
        }
    }

    /// Resetting the array slice information set by the incremental GC's marking or updating phase.
    /// As stabilization stops the incremental GC, the tag can be cleared.
    /// Even if the GC would resume operation, clearing the array slice information would not
    /// corrupt GC, only implyinf the rescanning or re-updating the array.
    fn clear_array_slicing(value: Value) {
        unsafe {
            assert!(value.is_obj());
            if value.tag() >= TAG_ARRAY_SLICE_MIN {
                let array = value.as_array();
                (*array).header.tag = TAG_ARRAY;
            }
        }
    }
}
