use crate::{
    constants::{GB, KB},
    stabilization::ic0_performance_counter,
};

/// Maximum amount of memory that is processed per increment.
/// The IC configures a maximum of 2 GB of stable memory that can
/// be accessed per message. We keep a conservative reserve of 1 GB.
const MEMORY_PROCESSING_LIMIT_PER_INCREMENT: u64 = GB as u64;

/// Execution limit for the graph copy increment.
/// Monitoring the message instruction counter and
/// the amount of processed memory.
/// The latter is relevant to avoid exceeding the limit
/// of how much stable memory be accessed in a message.
/// Optimization: Avoiding frequent repeated calls to
/// `ic0_performance_counter()` as this incurs 200
/// instructions itself.
/// Heuristic approach: The logic only occasionally
/// synchronizes the actual instruction counter after a
/// defined granularity of work:
/// * A certain amount of memory has been processed:
///     - During stabilization: Written stable memory.
///     - During destabilization: Allocated main memory
///       and cleared stable memory (during completion),
///   or,
/// * A certain number of `is_exceeded` calls has
///   been made.
/// Once the limit has been exceeded, the heuristics
/// continuously returns exceeded until the monitoring
/// is reset.
pub struct ExecutionLimit {
    /// Instruction counter at the beginning of the measurement.
    initial_instruction_counter: u64,
    /// Limit of processed instructions since measurment start.
    instruction_limit: u64,
    /// Amount of processed memory before the measurement.
    initial_processed_memory: u64,
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

impl ExecutionLimit {
    /// Threshold on the number of `is_exceeded` calls since the
    /// last instruction counter synchronization.
    const CALL_THRESHOLD: usize = 1_000;
    /// Threshold on the amount of processed memory since the last
    /// instruction counter synchronization.
    const MEMORY_THRESHOLD: u64 = 256 * KB as u64;

    pub fn new(instruction_limit: u64) -> ExecutionLimit {
        ExecutionLimit {
            initial_instruction_counter: Self::instruction_counter(),
            instruction_limit,
            initial_processed_memory: 0,
            call_counter: 0,
            last_processed: 0,
            exceeded: false,
        }
    }

    pub fn is_exceeded(&mut self, processed_memory: u64) -> bool {
        debug_assert!(self.initial_processed_memory <= processed_memory);
        // Check the memory limit.
        if processed_memory - self.initial_processed_memory > MEMORY_PROCESSING_LIMIT_PER_INCREMENT
        {
            return true;
        }
        // Check the instruction limit.
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
            debug_assert!(self.initial_instruction_counter <= current);
            let elapsed = current - self.initial_instruction_counter;
            self.exceeded = elapsed > self.instruction_limit;
        }
        self.exceeded
    }

    pub fn reset(&mut self, instruction_limit: u64, processed_memory: u64) {
        *self = Self::new(instruction_limit);
        self.initial_processed_memory = processed_memory;
    }

    fn instruction_counter() -> u64 {
        unsafe { ic0_performance_counter(0) }
    }
}
