use crate::{
    gc::incremental::{
        partition_map::{PartitionMap, MAX_PARTITIONS},
        Limits, Phase, PARTITION_MAP, PHASE,
    },
    types::*,
    visitor::visit_pointer_fields,
};

use super::INCREMENT_LIMIT;

pub struct UpdatingIncrement<'a> {
    steps: usize,
    limits: &'a Limits,
    partition_map: &'a mut PartitionMap,
    partition_index: &'a mut usize,
    scan_address: &'a mut Option<usize>,
}

impl<'a> UpdatingIncrement<'a> {
    pub unsafe fn instance() -> UpdatingIncrement<'a> {
        if let Phase::Update(state) = &mut PHASE {
            UpdatingIncrement {
                steps: 0,
                limits: &state.limits,
                partition_map: PARTITION_MAP.as_mut().unwrap(),
                partition_index: &mut state.partition_index,
                scan_address: &mut state.scan_address,
            }
        } else {
            panic!("Invalid phase");
        }
    }

    pub unsafe fn run(&mut self) {
        while *self.partition_index < MAX_PARTITIONS {
            let partition = self.partition_map.get_partition(*self.partition_index);
            if !partition.is_free() && !partition.to_be_evacuated() {
                self.update_partition();
                if self.steps > INCREMENT_LIMIT {
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
            *self.scan_address = Some(partition.start_address());
        }
        let end_address = if self
            .partition_map
            .is_allocation_partition(*self.partition_index)
        {
            self.limits.free
        } else {
            partition.end_address()
        };
        while self.scan_address.unwrap() < end_address {
            let block = Value::from_ptr(self.scan_address.unwrap());
            if block.is_obj() {
                let original = self.scan_address.unwrap() as *mut Obj;
                if original.is_marked() {
                    self.update_fields(original);
                    if self.steps > INCREMENT_LIMIT {
                        // Keep mark bit and scan address to later resume updating more slices of this array
                        return;
                    }
                }
                original.unmark();
            }
            let size = block_size(self.scan_address.unwrap());
            *self.scan_address.as_mut().unwrap() += size.to_bytes().as_usize();
            assert!(self.scan_address.unwrap() <= end_address);
            self.steps += 1;
            if self.steps > INCREMENT_LIMIT {
                return;
            }
        }
    }

    unsafe fn update_fields(&mut self, object: *mut Obj) {
        debug_assert!(object.is_marked());
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.limits.base,
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
                    gc.steps += SLICE_INCREMENT as usize;
                    new_start
                } else {
                    (*array).header.raw_tag = mark(TAG_ARRAY);
                    gc.steps += (array.len() % SLICE_INCREMENT) as usize;
                    array.len()
                }
            },
        );
    }
}
