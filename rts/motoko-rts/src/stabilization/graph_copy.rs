use crate::memory::Memory;

pub mod time;

/// Generic graph copy from main memory (from-space) to stable memory (to-space).
/// The direction of copying is fixed but the memory layout used in the from-space
/// and the to-space flips when switching between serialization and deserialization.
/// `S`: Source address type (from-space, main memory).
/// `T`: Target address type (to-space, stable memory).
/// `P`: Pointer encoding type (e.g. `u32` or `u64`).
/// During serialization:
/// * Main memory = main memory layout, S = Value.
/// * Stable memory = stable memory layout, T = StableMemoryAddress.
/// During derialization:
/// * Main memory = stable memory layout, S = StableMemoryAddress.
/// * Stable memory = main memory layout, T = Value.
pub trait GraphCopy<S: Copy, T: Copy, P: Copy + Default> {
    /// Start the entire graph copy algorithm: Copy the object graph reachable from the `root` pointer.
    /// Use this as follows:
    /// ```
    /// copy_algorithm.start();
    /// while !copy_algorithm.is_completed() {
    ///     copy_algorthm.copy_increment();
    /// }
    /// ```
    fn start<M: Memory>(&mut self, mem: &mut M, root: S) {
        self.evacuate(mem, root);
    }

    /// Determin whether the copy algorithm is completed,
    /// i.e. a sufficient amount of copy increments has been invoked.
    fn is_completed(&self) -> bool;

    /// Complete the graph copy algorithm.
    fn complete(&mut self);

    /// Copy reachable objects in a time-bounded work step. with a synthetic time bound.
    /// This allows to spread the incremtnal graph copy where the work is
    /// split in multiple increments over multiple IC messages.
    fn copy_increment<M: Memory>(&mut self, mem: &mut M) {
        self.reset_time();
        while !self.is_completed() && !self.time_over() {
            self.scan(mem);
        }
        if self.is_completed() {
            self.complete();
        }
    }

    /// Reset the time at the beginning of a new copy increment.
    fn reset_time(&mut self);

    /// Determine whether the time of copy increment has been exceeded.
    fn time_over(&self) -> bool;

    /// Lazy evacuation of a single object.
    /// Triggered for each pointer that is patched in the `scan()` function.
    /// Determines whether the object has already been copied before, and if not,
    /// copies it to the target space.
    /// Returns the new target address of the object.
    fn evacuate<M: Memory>(&mut self, mem: &mut M, object: S) -> T {
        match self.get_forward_address(object) {
            Some(target) => target,
            None => {
                let target = self.copy(mem, object);
                self.set_forward_address(object, target);
                target
            }
        }
    }

    /// Check if the object has been forwarded.
    /// Returns `None` if not forwarded, or otherwise, the new target address.
    fn get_forward_address(&self, object: S) -> Option<T>;

    /// Mark the object as forwarded and record its new target address.
    fn set_forward_address(&mut self, object: S, target: T);

    /// Allocate the object in the to-space by bumping the `free` pointer.
    /// Copy its content to that target location using the encoding of the target layout.
    /// Notes:
    /// * The pointer values in the field are retained as source addresses.
    /// * The source and target layout must use the same size for addresses, e.g. 32-bit.
    /// * The allocator must be contiguously growing. Free space must be inserted when the
    ///   allocator uses internal fragmentation, e.g. for the partitioned heap.
    fn copy<M: Memory>(&mut self, mem: &mut M, object: S) -> T;

    /// Read an object at the `scan` position in the to-space, and patch all the pointer fields
    /// by translating the source pointer to the corresponding new target pointer by calling
    /// `evacuate()`.
    fn scan<M: Memory>(&mut self, mem: &mut M);
}
