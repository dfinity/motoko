//! The implementation of streaming serialisation
//!
//! When serialising Motoko stable variables to stable memory we used to first completely.
//! fill up an in-heap buffer and then copy that wholesale into stable memory. This can be
//! disadvategous for two reasons:
//!  - double copying
//!  - heap congestion (especially for the compactig collector)
//!
//! Instead now we'll 
//!
//!

// Layout of a stream node:
//
//      ┌────────────┬─────┬───────┬─────────┬─────────┬────────┬──────────┐
//      │ tag (blob) │ len │ ptr64 │ limit64 │ flusher │ filled │ cache... │
//      └────────────┴─────┴───────┴─────────┴─────────┴────────┴──────────┘
//
// We reuse the opaque nature of blobs (to Motoko) and stick Rust-related information
// into the leading bytes:
// - tag and len are blob metadata
// - ptr64 and limit64 are the next and past-end pointers into stable memory
// - flusher is the 

