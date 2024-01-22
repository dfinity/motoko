mod scan_stack;
pub mod stable_memory_access;

use crate::{
    memory::Memory,
    stabilization::deserialization::scan_stack::STACK_EMPTY,
    types::{FwdPtr, Tag, Value, TAG_ARRAY, TAG_ARRAY_SLICE_MIN, TAG_FWD_PTR},
    visitor::visit_pointer_fields,
};

use self::{scan_stack::ScanStack, stable_memory_access::StableMemoryAccess};

use super::{
    clear_stable_memory,
    graph_copy::{limit::InstructionLimit, GraphCopy},
    layout::{deserialize, StableValue, STABLE_NULL_POINTER},
    moc_null_singleton, moc_stabilization_instruction_limit,
};

pub struct Deserialization {
    from_space: StableMemoryAccess,
    scan_stack: ScanStack,
    stable_start: u64,
    stable_size: u64,
    stable_root: Option<Value>,
    limit: InstructionLimit,
}

/// Helper type to pass serialization context instead of closures.
pub struct DeserializationContext<'a, M: Memory> {
    pub deserialization: &'a mut Deserialization,
    pub mem: &'a mut M,
}

impl<'a, M: Memory> DeserializationContext<'a, M> {
    fn new(
        deserialization: &'a mut Deserialization,
        mem: &'a mut M,
    ) -> DeserializationContext<'a, M> {
        DeserializationContext {
            deserialization,
            mem,
        }
    }
}

/// Graph-copy-based deserialization.
/// Usage:
/// ```
/// let deserialization = Deserialization::start(mem, stable_start, stable_size);
/// while !deserialization.is_completed() {
///     deserialization.copy_increment();
/// }
/// ```
impl Deserialization {
    /// Start the deserialization, followed by a series of copy increments.
    pub fn start<M: Memory>(mem: &mut M, stable_start: u64, stable_size: u64) -> Deserialization {
        let from_space = StableMemoryAccess::open(stable_start, stable_size);
        let scan_stack = unsafe { ScanStack::new(mem) };
        let limit = InstructionLimit::new(unsafe { moc_stabilization_instruction_limit() });
        let mut deserialization = Deserialization {
            from_space,
            scan_stack,
            stable_start,
            stable_size,
            stable_root: None,
            limit,
        };
        deserialization.start(mem, StableValue::serialize(Value::from_ptr(0)));
        deserialization
    }

    pub fn get_stable_root(&self) -> Value {
        self.stable_root.unwrap()
    }

    fn is_null(value: StableValue) -> bool {
        value == STABLE_NULL_POINTER
    }

    fn encode_null() -> Value {
        unsafe { moc_null_singleton() }
    }

    unsafe fn scan_deserialized<
        'a,
        M: Memory,
        F: Fn(&mut DeserializationContext<'a, M>, Value) -> Value,
    >(
        context: &mut DeserializationContext<'a, M>,
        target_object: Value,
        translate: &F,
    ) {
        debug_assert!(target_object.is_obj());
        visit_pointer_fields(
            context,
            target_object.as_obj(),
            target_object.tag(),
            0,
            |context, field_address| {
                *field_address = translate(context, *field_address);
            },
            |context, slice_start, array| {
                const SLICE_INCREMENT: u32 = 127;
                debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                if array.len() - slice_start > SLICE_INCREMENT {
                    let new_start = slice_start + SLICE_INCREMENT;
                    // Remember to visit the next array slice:
                    // Update the tag and push the array back on the stack.
                    (*array).header.tag = new_start;
                    context
                        .deserialization
                        .scan_stack
                        .push(context.mem, target_object);
                    new_start
                } else {
                    // No further visits of this array. Restore the tag.
                    (*array).header.tag = TAG_ARRAY;
                    array.len()
                }
            },
        );
    }
}

impl GraphCopy<StableValue, Value, u32> for Deserialization {
    fn get_forward_address(&self, stable_object: StableValue) -> Option<Value> {
        let address = stable_object.to_stable_address();
        let tag = self.from_space.read::<Tag>(address);
        match tag {
            TAG_FWD_PTR => {
                let forward_object = self.from_space.read::<FwdPtr>(address);
                Some(forward_object.fwd)
            }
            _ => None,
        }
    }

    fn set_forward_address(&mut self, stable_object: StableValue, target: Value) {
        let address = stable_object.to_stable_address();
        let forward_object = FwdPtr {
            tag: TAG_FWD_PTR,
            fwd: target,
        };
        self.from_space.write(address, &forward_object);
    }

    fn copy<M: Memory>(&mut self, mem: &mut M, stable_object: StableValue) -> Value {
        unsafe {
            let target = deserialize(mem, &mut self.from_space, stable_object);
            if self.stable_root.is_none() {
                self.stable_root = Some(target);
            }
            self.scan_stack.push(mem, target);
            target
        }
    }

    /// Note:
    /// * The deserialized memory may contain free space at a partition end.
    fn scan<M: Memory>(&mut self, mem: &mut M) {
        let target_object = unsafe { self.scan_stack.pop() };
        debug_assert!(target_object != STACK_EMPTY);
        unsafe {
            Self::scan_deserialized(
                &mut DeserializationContext::new(self, mem),
                target_object,
                &|context, original| {
                    let old_value = StableValue::serialize(original);
                    if Self::is_null(old_value) {
                        Self::encode_null()
                    } else if original.is_ptr() {
                        context.deserialization.evacuate(context.mem, old_value)
                    } else {
                        original
                    }
                },
            );
        }
    }

    fn is_completed(&self) -> bool {
        unsafe { self.scan_stack.is_empty() }
    }

    fn complete(&mut self) {
        clear_stable_memory(self.stable_start, self.stable_size);
    }

    fn time_over(&self) -> bool {
        self.limit.is_exceeded()
    }

    fn reset_time(&mut self) {
        self.limit.reset();
    }
}
