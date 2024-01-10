use super::ic0_performance_counter;

pub struct Measurement {
    initial_instructions: u64,
}

impl Measurement {
    pub fn start() -> Measurement {
        Measurement {
            initial_instructions: Self::instruction_counter(),
        }
    }

    pub fn elapsed_instructions(&self) -> u64 {
        let current_instructions = Self::instruction_counter();
        debug_assert!(self.initial_instructions <= current_instructions);
        current_instructions - self.initial_instructions
    }

    fn instruction_counter() -> u64 {
        unsafe { ic0_performance_counter(0) }
    }
}
