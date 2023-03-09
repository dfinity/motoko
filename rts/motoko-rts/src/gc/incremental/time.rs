//! Time of the GC increment, bounded or unbounded.
//! Deterministically measured in synthetic steps.
//! Bounded time is used for incremental old generation collection.
//! Unbounded time is used for blocking young generation collection.

pub struct Time {
    steps: usize,
    limit: usize, // Sentinel `usize::MAX` denotes unlimited.
}

impl Time {
    pub fn limited(limit: usize) -> Time {
        assert!(limit < usize::MAX);
        Time { steps: 0, limit }
    }

    pub fn unlimited() -> Time {
        Time {
            steps: 0,
            limit: usize::MAX,
        }
    }

    pub fn tick(&mut self) {
        self.advance(1);
    }

    pub fn advance(&mut self, amount: usize) {
        if amount <= usize::MAX - self.steps {
            self.steps += amount;
        } else {
            self.steps = usize::MAX;
        }
    }

    pub fn is_over(&self) -> bool {
        self.steps > self.limit
    }
}
