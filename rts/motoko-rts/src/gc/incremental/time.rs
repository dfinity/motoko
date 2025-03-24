// Bounded time of the GC increment.
// Deterministically measured in synthetic steps.
pub struct BoundedTime {
    steps: usize,
    limit: usize,
}

impl BoundedTime {
    pub fn new(limit: usize) -> BoundedTime {
        BoundedTime { steps: 0, limit }
    }

    // TODO: Saturating add is probably no longer needed in 64-bit (if that makes it faster).
    pub fn tick(&mut self) {
        self.steps = usize::saturating_add(self.steps, 1);
    }

    pub fn advance(&mut self, amount: usize) {
        debug_assert!(amount > 0);
        self.steps = usize::saturating_add(self.steps, amount);
    }

    pub fn is_over(&self) -> bool {
        self.steps >= self.limit
    }
}
