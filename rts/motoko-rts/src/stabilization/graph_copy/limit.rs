use crate::{constants::KB, stabilization::ic0_performance_counter};

/// Instruction limit for the graph copy increment.
/// Monitoring the message instruction counter.
/// Optimization: Avoiding frequent repeated calls to
/// `ic0_performance_counter()` as this incurs 200
/// instructions itself.
/// Heuristic approach: The logic only occasionally
/// synchronizes the actual instruction counter after a
/// defined granularity of work:
/// * A certain amount of memory has been processed
///   (written stable memory during stabilization or
///    allocated main memory during destabilization), or,
/// * A certain number of `is_exceeded` calls has
///   been made.
/// Once the limit has been exceeded, the heuristics
/// continuously returns exceeded until the monitoring
/// is reset.
pub struct InstructionLimit {
    /// Instruction counter at the beginning of the measurement.
    start: u64,
    /// Limit of processed instructions since measurment start.
    limit: u64,
    // Only used for sporadic synchronization heuristics:
    /// Number of `is_exceeded` calls since the last instruction
    /// counter synchronization.
    call_counter: usize,
    /// Amount of processed memory since the last instruction
    /// counter synchronization.
    last_processed: u64,
    /// Denotes whether the limit has been exceeded.
    exceeded: bool,
}

impl InstructionLimit {
    /// Threshold on the number of `is_exceeded` calls since the
    /// last instruction counter synchronization.
    const CALL_THRESHOLD: usize = 1_000;
    /// Threshold on the amount of processed memory since the last
    /// instruction counter synchronization.
    const MEMORY_THRESHOLD: u64 = 256 * KB as u64;

    pub fn new(limit: u64) -> InstructionLimit {
        InstructionLimit {
            start: Self::instruction_counter(),
            limit,
            call_counter: 0,
            last_processed: 0,
            exceeded: false,
        }
    }

    pub fn is_exceeded(&mut self, processed_memory: u64) -> bool {
        // Sporadic instruction counter synchronization, see above.
        self.call_counter += 1;
        if processed_memory >= self.last_processed.saturating_add(Self::MEMORY_THRESHOLD)
            || self.call_counter >= Self::CALL_THRESHOLD
        {
            // Reset the heuristics counters.
            self.call_counter = 0;
            self.last_processed = processed_memory;
            // Check actual instruction counter.
            let current = Self::instruction_counter();
            debug_assert!(self.start <= current);
            let elapsed = current - self.start;
            self.exceeded = elapsed > self.limit;
        }
        self.exceeded
    }

    pub fn reset(&mut self, limit: u64) {
        *self = Self::new(limit);
    }

    fn instruction_counter() -> u64 {
        unsafe { ic0_performance_counter(0) }
    }
}
