use crate::{
    gc::incremental::{
        partition_map::{PartitionMap, MAX_PARTITIONS},
        Phase, Roots, INCREMENT_LIMIT, PARTITION_MAP, PHASE,
    },
    types::*,
    visitor::visit_pointer_fields,
};

pub struct UpdateIncrement<'a> {
    steps: &'a mut usize,
    heap_base: usize,
    partition_map: &'a mut PartitionMap,
    partition_index: &'a mut usize,
    scan_address: &'a mut Option<usize>,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn instance(steps: &'a mut usize) -> UpdateIncrement<'a> {
        if let Phase::Update(state) = &mut PHASE {
            UpdateIncrement {
                steps,
                heap_base: state.heap_base,
                partition_map: PARTITION_MAP.as_mut().unwrap(),
                partition_index: &mut state.partition_index,
                scan_address: &mut state.scan_address,
            }
        } else {
            panic!("Invalid phase");
        }
    }

    pub unsafe fn update_roots(&mut self, roots: Roots) {
        self.update_static_roots(roots.static_roots);
        self.update_continuation_table(roots.continuation_table);
    }

    unsafe fn update_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for index in 0..root_array.len() {
            let mutbox = root_array.get(index).as_mutbox();
            debug_assert!((mutbox as usize) < self.heap_base);
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= self.heap_base {
                (*mutbox).field = value.forward_if_possible();
            }
            *self.steps += 1;
        }
    }

    unsafe fn update_continuation_table(&mut self, continuation_table: Value) {
        if continuation_table.is_ptr() {
            visit_pointer_fields(
                self,
                continuation_table.get_ptr() as *mut Obj,
                continuation_table.tag(),
                self.heap_base,
                |_, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                },
                |gc, _, array| {
                    *gc.steps += array.len() as usize;
                    array.len()
                },
            );
        }
    }

    pub unsafe fn run(&mut self) {
        while *self.partition_index < MAX_PARTITIONS {
            let partition = self.partition_map.get_partition(*self.partition_index);
            if !partition.is_free() && !partition.to_be_evacuated() {
                self.update_partition();
                if *self.steps > INCREMENT_LIMIT {
                    return;
                }
            }
            *self.partition_index += 1;
            *self.scan_address = None;
        }
    }

    unsafe fn update_partition(&mut self) {
        let partition = self.partition_map.get_partition(*self.partition_index);
        if self.scan_address.is_none() {
            *self.scan_address = Some(partition.dynamic_space_start());
        }
        let end_address = partition.dynamic_space_end();
        while self.scan_address.unwrap() < end_address {
            let block = Value::from_ptr(self.scan_address.unwrap());
            if block.is_obj() {
                let original = self.scan_address.unwrap() as *mut Obj;
                if original.is_marked() {
                    self.update_fields(original);
                    if *self.steps > INCREMENT_LIMIT {
                        // Keep mark bit and scan address to later resume updating more slices of this array
                        return;
                    }
                }
                original.unmark();
            }
            let size = block_size(self.scan_address.unwrap());
            *self.scan_address.as_mut().unwrap() += size.to_bytes().as_usize();
            assert!(self.scan_address.unwrap() <= end_address);
            *self.steps += 1;
            if *self.steps > INCREMENT_LIMIT {
                return;
            }
        }
    }

    unsafe fn update_fields(&mut self, object: *mut Obj) {
        assert!(object.is_marked());
        assert!(object.tag() < TAG_ARRAY_SLICE_MIN);
        loop {
            // Loop over array slices and return if GC increment is exceeded.
            visit_pointer_fields(
                self,
                object,
                object.tag(),
                self.heap_base,
                |_, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                },
                |gc, slice_start, array| {
                    debug_assert!(array.is_marked());
                    const SLICE_INCREMENT: u32 = 128;
                    debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                    if array.len() - slice_start > SLICE_INCREMENT {
                        let new_start = slice_start + SLICE_INCREMENT;
                        (*array).header.raw_tag = mark(new_start);
                        *gc.steps += SLICE_INCREMENT as usize;
                        new_start
                    } else {
                        (*array).header.raw_tag = mark(TAG_ARRAY);
                        *gc.steps += (array.len() % SLICE_INCREMENT) as usize;
                        array.len()
                    }
                },
            );
            if object.tag() < TAG_ARRAY_SLICE_MIN || *self.steps > INCREMENT_LIMIT {
                return;
            }
        }
    }
}
