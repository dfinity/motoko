# The IC's Stable Memory API

The IC provides a very small set of functions for operating on stable memory:

```
ic0.stable_size : () -> (page_count : i32);                                 // *
ic0.stable_grow : (new_pages : i32) -> (old_page_count : i32);              // *
ic0.stable_write : (offset : i32, src : i32, size : i32) -> ();             // *
ic0.stable_read : (dst : i32, offset : i32, size : i32) -> ();              // *
```

(see https://sdk.dfinity.org/docs/interface-spec/index.html#system-api-stable-memory)

These grow memory and do bulk transfers between Wasm and stable
memory.  The `// *` means that they can be called in all contexts
(e.g. init, update, query etc).  Direct reads and writes of word-sized
data to/from the stack are not supported but can be emulated at cost.
The initial size of the stable memory is zero. The contents of fresh pages (after grow) is initially zero.

Note that, in this API, the client is responsible for growing (both
stable and wasm) memory before access by read or write (out-of-bounds
access will trap).

# Stable Memory Accesses

Direct stable memory accesses (`ExperimentalStableMemory` in Motoko's base library) has been deprecated.
Instead, [stable regions](StableRegions.md) are available to explicitly access stable memory.

However, generally, programmers do not need to use stable memory due to the support of orthogonal persistence,
see [Enhanced Orthogonal Persistence](OrthogonalPersistence.md).
