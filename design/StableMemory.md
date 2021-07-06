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
   decode<T>(b : Blob) : T // or, if supportable, ?T

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

The raw API of the previous section might be workable but is totally unsafe, allowing writes to arbitrary offsets, possible corrupting data, and reads from random offset,
producing unpredictable behaviour.

Maybe a stable `ref T` type is a safer abstraction for stable memory?

* type `ref T`:  represent a fixed address in stable memory. The address stores current offset in stable memory of contents of r (T must be stable).
* `ref v` : allocates the next address, storing the serialized blob of `v` in stable memory, writing address of that blob into `r`, returns the address.
* `! r`: deserializes `v` from some blob in stable memory at current offset stored in stable reference `r`.
* `r := v` frees content blob `v` of r and serializes `v` to (re)allocated memory, updating
  offset stored at fixed address r.

Stable memory blobs are sized regions allocated from a free list. For security, we might want to zero discarded blob regions on free. Hopefully a simple best-fit, malloc-style allocator
that coalesces regions on free could be used for this.

We could also have write-once references, `stable T`, for immutable stable storage, e.g. for write-only logs.

If we deign references to be `stable`, but not `shared`, then we can store the (smallish) metadata pointing into stable memory in stable variables and cross our fingers that
pre- and post- upgrade hooks will have sufficient cycles to serialize/deserialize this smallish metadata.

In future, the GC could mark dead references as free, returning them to the allocator -
there is no need to collect StableMemory store for references as they are not shared and cannot, by typing, occur in stable memory (or be sent to other actors).

Drawbacks:

* No need for explicit serialization functions - hidden behind semantics of reading/writing from ref.
* Moderately complicated, potential to leak/fragment stable memory.
* Motoko specific, and no cross-language upgrade story, but that was true for our use of Candid(ish) stable variables too (since they extend Candid with mutable data).
* Returns ref contents from stable memory as a message reply would require redundant de- and re-serialization unless we optimize this to a bulk read from stable storage to memory.

# Maintaining existing Stable variables.

Stable memory is currently hidden behind the abstraction of stable
variables, which we will still need to maintain. The current
implementation of stable variables stores all variables as a
Candid(ish) record of _stable_ fields, starting at stable memory address 0 with
initial word encoding size (in bytes?) followed by contents.

Ideally, we would extend this so all user-defined StableMemory is stored at a low address, with
stable _variable_ data stored just beyond the currently used StableMemory
content on canister_pre_upgrade and canister_post_upgrade. That way the StableMemory area need not move, with stable variables simply serialized and appended in `canister_pre_upgrade` and deserialized and discarded in `canister_post_upgrade`, leaving the manual StableMemory unchanged.

Assuming stable variables always require non-zero bytes to encode, we should be able to devise a backwards compatible scheme for upgrading from pre-StableMemory canisters to post-StableMemory canisters. Details to be designed.

Stable Memory Format

| empty                   pages == 0
| size ; blob             pages > 0 && |size;blob| == pages * pagesize
| size ; blob ; max       pages > 0 && |size;blob| + 4 < pages * pagesize
| 0x0000; max


on pre_upgrade

match stable_var with
   null ->
    if pages == 0 return;
    let max = heap
    let word0 = ReadWord(0);
    if necessary, grow mem to max+7
    mem.swap([0..7], [max,max+7])
    mem.writeWord(0, 0x0000)
    mem.writeWord(4, max)
 | ? v
    let blob = serialize(v);
    let size : Word32 = |blob|;
    let max = heap;
    if necessary, grow mem to max + 4 + size + 4
    swap(mem, [0..4+size+4], [max, max+4+size+4])
    mem.writeWord(0, size)
    mem.writeWord(4, blob)
    mem.writeWord(8 + size, max)

on post_upgrade

if pages == 0
  heap := 0;
  return None
if pages > 0 && size != 0 then
  if pages * numpages = size + 4
    let max = 0
    heap := max
    Some deserialize(4)
  else
    let max = ReadWord(4+size);
    heap := max;
    swap(mem,[0..size+4],[max-size+4, max])
    Some (deserialize(max+4))
else
  max = ReadWord(4);
  heap = max;
  swap(mem,[0..7],[max-7,max])
  None

