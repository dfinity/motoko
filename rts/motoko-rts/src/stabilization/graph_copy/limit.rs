use crate::stabilization::ic0_performance_counter;

/// Instruction limit for the graph copy increment.
/// Monitoring the message instruction counter.
pub struct InstructionLimit {
    limit: u64,
}

impl InstructionLimit {
    pub fn new(limit: u64) -> InstructionLimit {
        InstructionLimit { limit }
    }

    pub fn is_exceeded(&self) -> bool {
        Self::instruction_counter() > self.limit
    }

    fn instruction_counter() -> u64 {
        unsafe { ic0_performance_counter(0) }
    }
}
