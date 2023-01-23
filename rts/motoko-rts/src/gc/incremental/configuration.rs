//! Incremental GC configuration constants.
//!
//! Distinguishing between debug and release build:
//! * Debug build: Tuned for frequent fine-grained GC.
//! * Release build: Tuned for production performance.

/// Size of each partition in the heap.
#[cfg(not(debug_assertions))]
pub const PARTITION_SIZE: usize = 32 * 1024 * 1024;
#[cfg(debug_assertions)]
pub const PARTITION_SIZE: usize = 2 * 1024 * 1024;

/// Fraction of non-garbage in a partition. Threshold for evacuating a partition.
#[cfg(not(debug_assertions))]
pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.35;
#[cfg(debug_assertions)]
pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.95;

/// GC scheduling threshold, relative growth since last GC run.
pub const HEAP_GROWTH_THRESHOLD: f64 = 0.65;

/// Extra schedule of the GC when this memory occupation is exceeded.
pub const CRITICAL_MEMORY_LIMIT: usize = usize::MAX - 2 * PARTITION_SIZE;
