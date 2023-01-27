/// Limits on the number of steps performed in a GC increment.
const GC_INCREMENT_TIME_LIMIT: usize = 1_500_000;

// Bounded time of the GC increment.
// Deterministically measured in synthetic steps.
pub struct BoundedTime {
    steps: usize,
    limit: usize,
}

impl BoundedTime {
    pub fn increment_time() -> BoundedTime {
        Self::new(GC_INCREMENT_TIME_LIMIT)
    }

    pub fn new(limit: usize) -> BoundedTime {
        BoundedTime { steps: 0, limit }
    }

    pub fn tick(&mut self) {
        self.steps += 1;
    }

    pub fn advance(&mut self, amount: usize) {
        self.steps += amount;
    }

    pub fn is_over(&self) -> bool {
        self.steps > self.limit
    }
}
