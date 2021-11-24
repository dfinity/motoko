use super::heap::{create_motoko_heap, MotokoHeap};
use super::utils::ObjectIdx;
use super::TestHeap;

use motoko_rts::page_alloc::PageAlloc;

use oorandom::Rand32;

fn rand_bool(rng: &mut Rand32) -> bool {
    rng.rand_range(0..2) == 1
}

pub(super) fn generate(seed: u64, max_small_objects: u32, max_large_objects: u32) -> TestHeap {
    let mut rng = Rand32::new(seed);

    let n_small_objects = rng.rand_range(0..max_small_objects + 1);
    let n_large_objects = rng.rand_range(0..max_large_objects + 1);
    let n_total_objects = n_small_objects + n_large_objects;

    let all_objects = (0..n_total_objects);

    let roots: Vec<ObjectIdx> = all_objects
        .clone()
        .filter_map(|obj_idx| {
            if rand_bool(&mut rng) {
                Some(obj_idx)
            } else {
                None
            }
        })
        .collect();

    let heap: Vec<(ObjectIdx, Vec<ObjectIdx>)> = all_objects
        .clone()
        .map(|obj_idx| {
            let n_fields = if obj_idx < n_small_objects {
                // Small object
                rng.rand_range(0..100)
            } else {
                // Large object. Allocate min. amount of fields to make a large object, to avoid
                // allocation huge amounts.
                // 12,285 = large object size (12,288)
                //              - 1 (object header)
                //              - 1 (array size field)
                //              - 1 (object tag field)
                12285
            };

            let field_values = (0..n_fields)
                .map(|_field_idx| rng.rand_range(0..n_total_objects))
                .collect::<Vec<ObjectIdx>>();

            (obj_idx, field_values)
        })
        .collect();

    // Same as roots
    let continuation_table: Vec<ObjectIdx> = all_objects
        .filter_map(|obj_idx| {
            if rand_bool(&mut rng) {
                Some(obj_idx)
            } else {
                None
            }
        })
        .collect();

    TestHeap {
        heap,
        roots,
        continuation_table,
    }
}
