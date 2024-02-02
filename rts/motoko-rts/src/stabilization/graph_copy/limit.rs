use crate::stabilization::ic0_performance_counter;

/// Instruction limit for the graph copy increment.
/// Monitoring the message instruction counter.
pub struct InstructionLimit {
    start: u64,
    limit: u64,
}

impl InstructionLimit {
    pub fn new(limit: u64) -> InstructionLimit {
        InstructionLimit {
            start: Self::instruction_counter(),
            limit,
        }
    }

    pub fn is_exceeded(&self) -> bool {
        let current = Self::instruction_counter();
        debug_assert!(self.start <= current);
        let elapsed = current - self.start;
        elapsed > self.limit
    }

    pub fn reset(&mut self) {
        self.start = Self::instruction_counter();
    }

    fn instruction_counter() -> u64 {
        unsafe { ic0_performance_counter(0) }
    }
}
