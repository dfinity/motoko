/// Bounded time of the graph copy increment.
/// Deterministically measured in synthetic steps.
// TODO: Unify with the bounded time used in the incremental GC.
pub struct BoundedTime {
    steps: usize,
    limit: usize,
}

impl BoundedTime {
    pub fn new(limit: usize) -> BoundedTime {
        BoundedTime { steps: 0, limit }
    }

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

    pub fn reset(&mut self) {
        self.steps = 0;
    }
}
