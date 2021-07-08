# Stable Memory API

The current implementation of stable variables is based on
serialization and deserialization of all stable data on upgrade. This
clearly doesn't scale to large amounts of stable data as there may not
be enough cycles to perform (de)serialization.

To avoid this upgrade hazard, some Rust canisters with low-level API
access, and large stable memory footprints, arrange to store their
persistent data in stable memory at all times, using either a custom
binary encoding or a mixture of candid and raw binary.

To provide more fine-grained access to stable memory we propose
extending the existing stable variable implementation with an orthogonal,
library providing (almost) direct access to the IC Stable Memory API.

Since the implementation of stable variables itself makes use of
stable memory, some coordination between the old and new
abstractions is required.


# The IC's Stable Memory API

The IC provides a very small set of operations.

```
ic0.stable_size : () -> (page_count : i32);                                 // *
ic0.stable_grow : (new_pages : i32) -> (old_page_count : i32);              // *
ic0.stable_write : (offset : i32, src : i32, size : i32) -> ();             // *
ic0.stable_read : (dst : i32, offset : i32, size : i32) -> ();              // *
```

These grow memory and do bulk transfers between Wasm and stable
memory.  The `// *` means that they can be called in all contexts
(e.g. init, update, query etc).  Direct reads and writes of word-sized
data to/from the stack are not supported but can be emulated at cost.
The contents of fresh pages (after grow) is initially zero.

Note that, in this API, the client is responsible for growing (both
stable and wasm) memory before access by read or write (out-of-bounds
access will trap).

# A minimal Stable Memory API

The minimal Motoko prims could be:

```
module StableMemory {
  writeNat8 : (offset : Nat8, n : Nat8) -> ();
  readNat8 : (offset : Nat8) -> Nat8;
  // etc for all scalar prim types.
  ...
  writeBlob : (offset : Nat32, b : Blob) -> (); // write contents of blob to memory, growing stable memory region to at least offset+size
  readBlob : (offset : Nat32, size : i32) -> Blob
     // read Blob contents from memory at [offset,..,offset+size-1] into fresh blob, growing stable memory region on demand (and padding with zeros).
}
```

Each operation will maintain an internal global i32 `max` bounding the maximum address read or written (from above), initially 0, always <= 0xFFFFFFF0
(since we need at least two words to store 0x00 and `max` in raw stable memory on upgrade.)
`max` is really the size of the user-accessed address range.

```
fun writeNat8(offset, b) =
   max := Max(max, offset + 1);
   // grow stable memory to include offset
   mem[offset] := b;

fun readNat8(offset, b) =
   max := Max(max, offset + 1);
   // grow stable memory to include offset
   mem[offset]
```

On top of this basic API, users should be able to build more interesting higher-level APIs for pickling user-defined data.

REMARK:

Actually implementing the sketched assignment in IRL involves writing
the contents to memory and then copying stable memory - even for
individual words - this could be optimized by an improved system API
offering direct load and stores from/to the stack:

```
ic0.stable_write_i32 : (offset : i32, val: i32) -> ();   // *
ic0.stable_read_i32 : (offset : i32, size : i32) -> i32; // *
// similarly for i64, f32, f64
```

## Bikeshedding:

It might be preferable to arrange the API by type, with one nested module per type:

```
module StableMemory {
  Nat8 : module {
    write : (offset : Nat8, n : Nat8) -> ();
    read : (offset : Nat8) -> Nat8;
  }
  // etc for all scalar prim types.
  ...
  Blob : {
    write : (offset : Nat32, b : Blob) -> ();
    read : (offset : Nat32, size : i32) -> Blob
  }
}
```
(I think the compiler will still optimize these nested calls to known
function calls, but it would be worth checking).

# Maintaining existing Stable Variables.

Stable memory is currently hidden behind the abstraction of stable
variables, which we will still need to maintain. The current
implementation of stable variables stores all variables as a
Candid(ish) record of _stable_ fields, starting at stable memory address 0 with
initial word encoding size (in bytes?) followed by contents.

Starting from a clean slate, we would extend this so all user-defined StableMemory is
stored at a low address, with _stable variable_ data stored just
beyond the currently used StableMemory content on canister_pre_upgrade
and canister_post_upgrade. That way the StableMemory area need not
move, with stable variables simply serialized and appended in
`canister_pre_upgrade` and deserialized and discarded in
`canister_post_upgrade`, leaving the manual StableMemory unchanged.

For backwards compatibility reasons, we can't do that.

Luckily, stable variables always require non-zero bytes to encode, we
should be able to devise a backwards compatible scheme for upgrading
from pre-StableMemory canisters to post-StableMemory
canisters, as follows.

During execution, abstract stable memory (StableMemory) is aligned
with IC stable memory, at address 0, for reasonable efficiency (apart
from maintaining `max`).

During upgrade, we compute the size of the stable variable blob, shift
that portion of StableMemory to the end of user memory (growing if
necessary) and write the old format into the space just freed,
followed by the size of StableMem.

If there are no stable variables, we shift the first two words of
StableMemory to the end, and simply write the 32-bit word 0x0,
followed by the 32-bit size of `max`, of StableMemory.

In post_upgrade, we reverse this process to discover any stable vars and size of StableMemory,
taking care to zero the memory vacated by shifting back the initial portion of StableMem
(so that reads beyond `max` see what they would expect).

Stable memory layout (during execution):

*  aligned with stable-memory, with global max holding max size of memory read or written in i32 (initially 0).
*  each write adjusts max_size and grows memory if necessary
*  each read adjusts max_size and grows memory if necessary, reading 0 for fresh locations.

Stable memory layout (between upgrades) :

```
match stable_vars, mem[0..max-1] with
  None, mem[0..max-1] ->
    if ic.stable_size() == 0 then
    assert max == 0;
    []
    else
    // stable grow mem to page including address | 4 + 4 + max - 1 |
    [word(0x0)@@max@@mem[4 + 4, ..., max - 1]@@mem[0, ..., 4 + 4 -1 ]
  Some v, mem[0..max-1] ->
    let blob = serialise(v) in
    // stable grow page including address | 4 + |blob| + 4 + max |
    [size(serialize(v))@@blob@@max@@mem[4 + |blob| + 4, ..., max -1 ]@@mem[0, ..., 4 + |blob| + 4  -1]
```

On pre_upgrade
```
func pre_upgrade(v_opt, max : word) =
  match v_opt with
    null ->
      if ic.stable_size() == 0 then
         assert (max == 0);
         ()
      else
      // if necessary, grow mem to size max + 8
      // save first 2 words to end
      mem[max, .., max + 7] = mem[0..7];
      // write marker 0x0 and max to first two words
      mem.writeWord(0, 0x0)
      mem.writeWord(4, max)
  | Some v ->
    let blob = serialize(v);
    let size : Word32 = |blob|;
    assert (size <> 00)
    //if necessary, grow mem to max + 4 + size + 4
    // shift first 4 + size + 4 words to end
    mem[max, max + 4 + size + 4 - 1] = mem[0.. 4 + size + 4  -1];
    mem.writeWord(0, size)       // save non-zero size of blob
    mem.writeBlob(4, blob)       // save blob
    mem.writeWord(4 + size, max) // save max
```

on post_upgrade

```
// deserializes any stable variables from beginning of store and
// and restores stable memory to address [0..max)
// returns first free address (max) of stable mem and stable value.
fun post_upgrade () : (word * value option) =
  let pages = ic0.stable_size() in
  if pages == 0 then
    0, None
  else
    let size = mem.ReadWord(0) in // read zero or size of stable value
    if size != 0 then
      // grow memory to 4 + size + 4 + max
      let v = deserialize(4) in
      let max = ReadWord(4 + size) in
      // swap stable var area & max with end of raw mem area, restoring stable mem
      mem[0..4 + size + 4 - 1] := mem[max, max + 4 + size + 4 -  1]
        mem[max, ..., max + 4 + size + 4 - 1] := [0, ..., 0]; // null memory
        max, Some v
    else // size == 0, no stable var
      let max = ReadWord(4) in
      // grow memory to 4 + size + 4 + max
      mem[0..7] := [max, max + 7]
      mem[max, max + 7 ] := [0, ..., 0];
      max, None
```

Note that we still need to do some work during updgrade and postupgrade, but if stable variables and user-defined pre/post upgrade hooks are
avoided, then the work is minimal (swap two words) and highly unlikely to exhaust cycle budget.


REMARK: With hindsight, it was probably a mistake not to include a versioning word in stable memory. Should we introduce that now while we still can?
