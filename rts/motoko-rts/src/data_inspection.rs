//! Generic heap data inspection.
//!
//! This requires precise object tagging and is thus only supported in combination
//! with enhanced orthogonal persistence.
//!
//! Data inspection returns the set of live objects (stable and flexible) together
//! with the root set, by preserving the graph structure. A custom binary encoding
//! is used that essentially maps the binary map of the objects, by skipping the
//! forwarding pointer of the incremental GC.
//!
//! Currently, for simplicity, only full heap inspection is realized. For better
//! scalability, incremental inspection can be implemented, see below.
//!
//! Binary format (EBNF):
//! ```
//! Format = Version RootSet Heap.
//! Version = `1: usize`.
//! RootSet = `length: usize` {`object_id: usize`}.
//! Heap = `object_id: usize` `object_tag: usize` `object_payload:
//! ```
//!
//! The object payload is organized as follows:
//! * The regular RTS object payload, with pointers replaced by object ids.
//! * The payload is always a multiple of the word size.
//! * For `Object`, the object size is prepended because the hash blob cannot directly be
//!   located in the stream.
//!
//! `object_id` are potentially synthetic identifiers of an objects. The ids are skewed,
//! to distinguish them from scalars. Currently, the `object_id` are heap pointers but
//! this would change with incremental inspection.
//!
//! `usize` is 64-bit little endian.
//!
//! Implementation:
//! * Currently, a separate mark bitmap is used for heap traversal during inspection. This
//!   bitmap is independent of the potentially other bitmaps used during incremental GC.
//! * For arrays, the tag can not be copied one-to-one from the heap object as it may
//!   temporarily store slicing information stored during the incremental GC.
//! * As usual, forwarding pointers of the incremental GC need to be resolved during heap
//!   inspection.
//! * A separate mark stack is needed during heap inspection. This stack additionally
//!   stores the array slicing information of the heap inspection, independent of the
//!   incremental GC.
//! * A simple stream buffer is used to serialize the binary result of the heap inspection.
//!   The buffer is represented as a linked list of blobs that is finally copied to a combined
//!   single blob. This is because the size of the live set is not known in advance.
//!
//! FUTURE: Incremental Inspection
//! Incremental inspection can be realized in the future for scalability to larger heaps:
//! * It enables chunked data downloads in multiple messages without blocking other user
//!   messages. This is particularly important because the message response size is limited.
//! * It establishes a logical session where the client receives incremental heap changes
//!   without needing to refetch the full heap.
//! Possible implementation:
//! * Synthetic object ids need to be used that are independent of the address of the object.
//!   This is because objects are moved by the GC.
//! * A hash map can be used to map heap pointers to synthetic object ids. This map also serves
//!   for marking during heap traversal, such that a mark bitmap would no longer be needed.
//!   The pointers in the map are weak pointers that are updated by the GCs but will be removed
//!   from the map if the object is collected.
//! * On chunked data downloads, the object state can only be sent if all their contained
//!   pointers have been traversed. Otherwise, their state need to be transmitted on a
//!   subsequent download message.
//! * A pending list record the objects which state is ready to be sent in a next download.
//!   The pointers in the pending list are weak.
//! * Write barriers need to be extended to catch all mutator writes to pointers **and** scalars
//!   during a heap inspection session. The pointers of modified objects are recorded in a
//!   hash set, similar to the remembered set of the generational GCs. Again the pointers are
//!   treated as weak pointers.
//! * On incremental inspection, the runtime system resends the state of modified objects of
//!   the hash set in addition to a potentially next heap chunk of the pending list.
//!   The hash set is eventually cleared and the sent objects are removed from the pending list.
//! * On the client side, the object graph is updated for each resent object, while new objects
//!   are added.

mod mark_bitmap;
mod mark_stack;
mod stream;

use mark_bitmap::MarkBitmap;
use mark_stack::{MarkStack, StackEntry};
use motoko_rts_macros::ic_mem_fn;
use stream::Stream;

use crate::{
    gc::incremental::roots::{root_set, visit_roots, Roots},
    memory::{
        ic::{enhanced_memory::minimum_memory_capacity, get_aligned_heap_base},
        Memory,
    },
    types::{
        base_array_tag, block_size, is_array_or_slice_tag, is_object_tag, slice_tag, Array, Tag,
        Value, TAG_ARRAY_SLICE_MIN, TAG_OBJECT,
    },
    visitor::visit_pointer_fields,
};

const DATA_INSPECTION_VERSION: usize = 1;

#[ic_mem_fn]
pub unsafe fn inspect_data<M: Memory>(mem: &mut M) -> Value {
    let roots = root_set();

    let mut stream = Stream::new();
    write_header(&mut stream, mem, &roots);

    let mut traversal = HeapTraversal::new(
        mem,
        &mut stream,
        get_aligned_heap_base(),
        minimum_memory_capacity().as_usize(),
    );
    traversal.run(roots);

    stream.finalize(mem)
}

unsafe fn write_header<M: Memory>(stream: &mut Stream, mem: &mut M, roots: &Roots) {
    stream.write(mem, &DATA_INSPECTION_VERSION);
    stream.write(mem, &roots.len());
    for root in *roots {
        // Resolve incremental GC forwarding pointer in root set.
        let forwarded_root = (*root).forward_if_possible();
        stream.write(mem, &forwarded_root);
    }
}

struct HeapTraversal<'a, M: Memory + 'a> {
    mem: &'a mut M,
    stream: &'a mut Stream,
    mark_stack: MarkStack,
    mark_bitmap: MarkBitmap,
}

impl<'a, M: Memory + 'a> HeapTraversal<'a, M> {
    unsafe fn new(
        mem: &'a mut M,
        stream: &'a mut Stream,
        heap_base: usize,
        heap_end: usize,
    ) -> Self {
        let mark_stack = MarkStack::new(mem);
        let mark_bitmap = MarkBitmap::new(mem, heap_base, heap_end);
        Self {
            mem,
            stream,
            mark_stack,
            mark_bitmap,
        }
    }

    unsafe fn run(&mut self, roots: Roots) {
        self.add_roots(roots);
        self.traverse();
    }

    unsafe fn add_roots(&mut self, roots: Roots) {
        visit_roots(roots, 0, self, |traversal, field| {
            traversal.mark_object(*field);
        });
    }

    unsafe fn mark_object(&mut self, object: Value) {
        // The forwarding pointer of the incremental GC is resolved in the bitmap.
        if !self.mark_bitmap.is_marked(object) {
            self.mark_bitmap.mark(object);
            // Ignore the array slice information of the incremental GC.
            let tag = Self::base_object_tag(object.tag());
            let entry = StackEntry::new(object, tag);
            self.mark_stack.push(self.mem, entry);
        }
    }

    unsafe fn traverse(&mut self) {
        while !self.mark_stack.is_empty() {
            let entry = self.mark_stack.pop();
            self.dump(entry.object);
            self.mark_fields(entry);
        }
    }

    unsafe fn mark_fields(&mut self, entry: StackEntry) {
        visit_pointer_fields(
            self,
            entry.object.as_obj(), // Resolves the forwarding pointer.
            entry.tag,             // Potential array slice information for this heap traversal.
            0,
            |traversal, field_address| {
                let field_value = *field_address;
                traversal.mark_object(field_value);
            },
            |traversal, slice_start, array| traversal.slice_array(slice_start, array),
        );
    }

    unsafe fn slice_array(&mut self, slice_start: usize, array: *mut Array) -> usize {
        const SLICE_INCREMENT: usize = 128;
        debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
        if array.len() - slice_start > SLICE_INCREMENT {
            let new_start = slice_start + SLICE_INCREMENT;
            let object = Value::from_ptr(array as usize);
            let base_tag = array.base_tag();
            let next_slice = StackEntry {
                object,
                tag: slice_tag(base_tag, new_start),
            };
            self.mark_stack.push(self.mem, next_slice);
            new_start
        } else {
            array.len()
        }
    }

    unsafe fn dump(&mut self, value: Value) {
        // Resolve the forwarding pointer of incremental GC.
        let object = value.as_obj();
        // Ignore array slice information of incremental GC.
        let tag = Self::base_object_tag(object.tag());
        self.stream.write(self.mem, &tag);
        // Skip GC's forwarding pointer in the stream output.
        // Directly stream the object payload, using pointers as object ids.
        let object_address = object as usize;
        let length = block_size(object_address).to_bytes().as_usize();
        // Objects need the size prepended to enable stream reading.
        // This is because the object size is derived from the hash blob
        // that may occur after the object, unless one reorders the
        // heap traversal.
        if tag == TAG_OBJECT {
            let object_size = value.as_object().size();
            self.stream.write(self.mem, &object_size);
        }
        self.stream.raw_write(self.mem, object_address, length);
    }

    fn base_object_tag(tag: Tag) -> Tag {
        debug_assert!(is_object_tag(tag));
        if is_array_or_slice_tag(tag) {
            base_array_tag(tag)
        } else {
            tag
        }
    }
}
