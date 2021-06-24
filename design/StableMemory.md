# Stable Memory API

The current implementation of stable variables is based on
serialization and deserialization of all stable data on upgrade. This
clearly doesn't scale to large amounts of stable data as there may not
be enough cycles to perform (de)serialization.

To avoid this upgrade hazard, some Rust canisters with low-level API
access, and large stable memory footprints arrange to store their
persistent data in stable memory at all times, using either a custom
binary encoding or a mixture of candid and raw binary.

To provide more fine-grained access to stable memory we propose:

A) extending Motoko with access to (monomorphic) Candid serialization primitives.
   This is most easily achieved by extending the syntax with saturated serialization and deserialization
   constructs, along the lines of debug_show.

```
   encode<T>(v : T) : Blob
   decode<T>(b : Blob) : T

   (where T shared or, to allow isolated graphs, T stable).
```

The current implementations of these prims embed the type table with the value. This means blobs are larger, but also self-describing, allowing per blob upgrade.



B) Surfacing stable memory API

```
ic0.stable_size : () -> (page_count : i32);                                 // *
ic0.stable_grow : (new_pages : i32) -> (old_page_count : i32);              // *
ic0.stable_write : (offset : i32, src : i32, size : i32) -> ();             // *
ic0.stable_read : (dst : i32, offset : i32, size : i32) -> ();              // *
```

These grow memory and do bulk transfers between Wasm and stable memory.
The `// *` means that they can be called in all contexts (e.g. init, update, query etc).
Direct reads and writes of word-sized data to/from the stack are not supported but can be emulated at cost.

The Minimal Motoko prims could be:

```
module StableMemory {
  stableSize : () -> (page_count : Nat32); // hidden?
  stableGrow : (new_pages : Nat32) -> (old_page_count : Nat32); // hidden?
  stableWrite : (offset : Nat32, b : Blob) -> (); // write size + contents of blob to memory, growing stable memory region on demand
  stableRead : (offset : Nat32) -> Blob // read Blob size & contents from memory at offset into fresh blob, trapping if out-of-bounds
}
```

Q:
* should we read/write size + contents or read/write size and contents separately?
* should we have a more streaming API, that returns the next free offset?
* should we use a more malloc style API, returning opaque addresses. If so, should we be able to update their blob content (size permitting) and even free them? Should we use GC to discover unreachable blobs and free them? If opaque addresses aren't sharable, we don't need to worry about gc'ing the blob themselves, but they can't be stored in stable vars either unless we allow Candid(ish) stable data to additionally include opaque address.

# Stable Ref types?

Maybe a stable ref type is a safer abstraction for stable memory.
* ref r i a fixed address in stable memory, stores current word offset in stable memory of contents of r.
* ref v : allocates the next address, stores the serialization of v in stable memory, writing address of that blob into r, returns the address.
* ! r: deserializes v from stable memory at current offset stored in r
* r := v' frees content blob v of r and serializes v' to (re)allocated memory, updating
  offset stored at fixed address r.


# Stable variables

Stable memory is currently hidden behind the abstraction of stable
variables, which we will still need to maintain. The current
implementation of stable variables stores all variables as a
Candid(ish) record of _stable_ fields, starting at stable memory address 0 with
initial word encoding size (in bytes?) followed by contents.

Ideally, we would extend this so all user-defined StableMemory is stored at a low address, with
stable _variable_ data stored just beyond the currently used StableMemory
content on canister_pre_upgrade and canister_post_upgrade. That way the StableMemory area need not move, with stable variables simply serialized and appended in `canister_pre_upgrade` and deserialized and discarded in `canister_post_upgrade`, leaving the manual StableMemory unchanged.

Assuming stable variables always require non-zero bytes to encode, we should be able to devise a backwards compatible scheme for upgrading from pre-StableMemory canisters to post-StableMemory canisters. Details to be designed.







