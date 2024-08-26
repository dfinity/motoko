use crate::{
    constants::KB,
    stabilization::{
        ic0_performance_counter, moc_stabilization_instruction_limit,
        moc_stable_memory_access_limit,
    },
};

/// Graph copy limits depending on the IC message type.
/// Explicit stabilization and destabilization increments run as update messages
/// with lower limits, while the graph copy during the actual upgrade can use
/// higher limits.
#[derive(Clone, Copy)]
pub struct ExecutionLimits {
    /// Limit of the instructions executed during a graph copy increment.
    instruction_limit: u64,
    /// Limit of read or written stable memory during a graph copy increment.
    stable_memory_access_limit: u64,
}

impl ExecutionLimits {
    /// Determine the limits for the current IC message.
    pub fn determine() -> ExecutionLimits {
        ExecutionLimits {
            instruction_limit: unsafe { moc_stabilization_instruction_limit() },
            stable_memory_access_limit: unsafe { moc_stable_memory_access_limit() },
        }
    }
}

/// Execution monitor for the graph copy increment.
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
pub struct ExecutionMonitor {
    /// Limits depending on IC message type (upgrade or update).
    limits: ExecutionLimits,
    /// Instruction counter at the beginning of the measurement.
    initial_instruction_counter: u64,
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

impl ExecutionMonitor {
    /// Threshold on the number of `is_exceeded` calls since the
    /// last instruction counter synchronization.
    const CALL_THRESHOLD: usize = 1_000;
    /// Threshold on the amount of processed memory since the last
    /// instruction counter synchronization.
    const MEMORY_THRESHOLD: u64 = 256 * KB as u64;

    pub fn new() -> ExecutionMonitor {
        ExecutionMonitor {
            limits: ExecutionLimits::determine(),
            initial_instruction_counter: Self::instruction_counter(),
            initial_processed_memory: 0,
            call_counter: 0,
            last_processed: 0,
            exceeded: false,
        }
    }

    pub fn is_exceeded(&mut self, processed_memory: u64) -> bool {
        debug_assert!(self.initial_processed_memory <= processed_memory);
        // Check the memory limit.
        if processed_memory - self.initial_processed_memory > self.limits.stable_memory_access_limit
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
            self.exceeded = elapsed > self.limits.instruction_limit;
        }
        self.exceeded
    }

    pub fn reset(&mut self, processed_memory: u64) {
        *self = Self::new();
        self.initial_processed_memory = processed_memory;
    }

    fn instruction_counter() -> u64 {
        unsafe { ic0_performance_counter(0) }
    }
}
