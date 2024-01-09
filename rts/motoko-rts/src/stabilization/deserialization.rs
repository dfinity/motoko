mod scan_stack;
pub mod stable_memory_access;

use crate::{
    memory::Memory,
    stabilization::deserialization::scan_stack::STACK_EMPTY,
    types::{block_size, FwdPtr, Tag, Value, TAG_FWD_PTR},
    visitor::visit_pointer_fields,
};

use self::{scan_stack::ScanStack, stable_memory_access::StableMemoryAccess};

use super::{
    clear_stable_memory,
    graph_copy::time::BoundedTime,
    graph_copy::GraphCopy,
    layout::{deserialize, StableValue, STABLE_NULL_POINTER},
    moc_null_singleton, COPY_TIME_LIMIT,
};

pub struct Deserialization<'a, M: Memory + 'a> {
    mem: &'a mut M,
    from_space: StableMemoryAccess,
    scan_stack: ScanStack,
    stable_start: u64,
    stable_size: u64,
    stable_root: Option<Value>,
    time: BoundedTime,
}

/// Graph-copy-based deserialization.
/// Usage:
/// ```
/// let deserialization = Deserialization::start(mem, stable_start, stable_size);
/// while !deserialization.is_completed() {
///     deserialization.copy_increment();
/// }
/// ```
impl<'a, M: Memory + 'a> Deserialization<'a, M> {
    /// Start the deserialization, followed by a series of copy increments.
    pub fn start(mem: &'a mut M, stable_start: u64, stable_size: u64) -> Deserialization<'a, M> {
        let from_space = StableMemoryAccess::open(stable_start, stable_size);
        let scan_stack = unsafe { ScanStack::new(mem) };
        let time = BoundedTime::new(COPY_TIME_LIMIT);
        let mut deserialization = Deserialization {
            mem,
            from_space,
            scan_stack,
            stable_start,
            stable_size,
            stable_root: None,
            time,
        };
        deserialization.start(StableValue::serialize(Value::from_ptr(0)));
        deserialization
    }

    pub fn complete(&mut self) -> Value {
        clear_stable_memory(self.stable_start, self.stable_size);
        self.stable_root.unwrap()
    }

    fn is_null(value: StableValue) -> bool {
        value == STABLE_NULL_POINTER
    }

    fn encode_null() -> Value {
        unsafe { moc_null_singleton() }
    }

    unsafe fn scan_deserialized<C, F: Fn(&mut C, Value) -> Value>(
        context: &mut C,
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
            |_, _, array| array.len(),
        );
    }
}

impl<'a, M: Memory + 'a> GraphCopy<StableValue, Value, u32> for Deserialization<'a, M> {
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

    fn copy(&mut self, stable_object: StableValue) -> Value {
        unsafe {
            let target = deserialize(self.mem, &mut self.from_space, stable_object);
            if self.stable_root.is_none() {
                self.stable_root = Some(target);
            }
            self.scan_stack.push(self.mem, target);
            let size = block_size(target.get_ptr() as usize).to_bytes().as_usize();
            self.time.advance(size);
            target
        }
    }

    /// Note:
    /// * The deserialized memory may contain free space at a partition end.
    fn scan(&mut self) {
        let target_object = unsafe { self.scan_stack.pop() };
        debug_assert!(target_object != STACK_EMPTY);
        unsafe {
            Self::scan_deserialized(self, target_object, &|context, original| {
                context.time.tick();
                let old_value = StableValue::serialize(original);
                if Self::is_null(old_value) {
                    Self::encode_null()
                } else if original.is_ptr() {
                    context.evacuate(old_value)
                } else {
                    original
                }
            });
        }
    }

    fn is_completed(&self) -> bool {
        unsafe { self.scan_stack.is_empty() }
    }

    fn time_over(&self) -> bool {
        self.time.is_over()
    }

    fn reset_time(&mut self) {
        self.time.reset();
    }
}
