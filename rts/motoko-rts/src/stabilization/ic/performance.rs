use crate::stabilization::ic0_performance_counter;

pub struct InstructionMeter {
    total_elapsed: u64,
    start_offset: Option<u64>,
}

impl InstructionMeter {
    pub fn new() -> InstructionMeter {
        InstructionMeter {
            total_elapsed: 0,
            start_offset: None,
        }
    }

    pub fn start(&mut self) {
        assert!(self.start_offset.is_none());
        self.start_offset = Some(Self::instruction_counter());
    }

    pub fn stop(&mut self) {
        let start = self.start_offset.unwrap();
        let stop = Self::instruction_counter();
        debug_assert!(start <= stop);
        self.total_elapsed += stop - start;
        self.start_offset = None;
    }

    pub fn total_elapsed(&self) -> u64 {
        assert!(self.start_offset.is_none());
        self.total_elapsed
    }

    fn instruction_counter() -> u64 {
        unsafe { ic0_performance_counter(0) }
    }
}
