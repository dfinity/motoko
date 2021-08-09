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
stable memory, some coordination between these two alternative, co-existing
interfaces to IC stable memory is required.


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

# A minimal Stable Memory API

The minimal Motoko prims could be:

```
module StableMemory {
  size : () -> (logical_page_count : i32); // <= ic0.stable_size()
  grow : () -> (new_pages : i32) -> (old_logical_page_count : i32);
  loadNat8 : (offset : Nat32) -> Nat8;
    // traps outside logical address space
  storeNat8 : (offset : Nat32, n : Nat8) -> ();
    // traps outside logical address space
  ...
  loadBlob : (offset : Nat32, size : i32) -> Blob
     // read Blob contents from memory at [offset,..,offset+size-1] into fresh blob, trapping if exceeding logical address space
  storeBlob : (offset : Nat32, b : Blob) -> (); // write contents of blob to memory, trapping if exceeding logical address space
}
```

NOTE: Motoko's `Nat32` value are always boxed - it might be more efficient to use `Nat` which is unboxed for 30(?)-bit values.


```
fun loadNat8(offset, b) =
   assert (offset < StableMemory.size() * wasm_page_size);
   mem[offset]

fun storeNat8(offset, b) =
   assert (offset < StableMemory.size() * wasm_page_size);
   mem[offset] := b

```

(To avoid overflow on the rhs, we could implement the check as `assert ((offset >> 6) < StableMemory.size())`.)

On top of this basic API, users should be able to build more
interesting higher-level APIs for pickling user-defined data.

REMARK:

Actually implementing the sketched assignments using the existing IC
API involves writing the contents to memory and then copying stable
memory - even for individual words - this could be optimized by an
improved system API offering direct load and stores from/to the stack:

```
ic0.stable_write_i32 : (offset : i32, val: i32) -> ();   // *
ic0.stable_read_i32 : (offset : i32, size : i32) -> i32; // *
// similarly for i64, f32, f64
```

## Bikeshedding:

It might be preferable to arrange the API by type, with one nested
module per type:

```
module StableMemory {
  Nat8 : module {
    load : (offset : Nat32) -> Nat8;
    store : (offset : Nat32, n : Nat8) -> ();
  };
  Nat16 : module {
    load : (offset : Nat32) -> Nat16;
    store : (offset : Nat32, n : Nat16) -> ();
  };
  // uniformly for all scalar prim types.
  ...
  Blob : module {
    read: (offset : Nat32, size : i32) -> Blob
    write : (offset : Nat32, b : Blob) -> ();
  }
}
```

(I think the compiler will still optimize these nested calls to known
function calls, but it would be worth checking).

# Maintaining existing Stable Variables.

Stable memory is currently hidden behind the abstraction of stable
variables, which we will still need to maintain. The current
implementation of stable variables stores all variables as a
Candidish record of _stable_ fields, starting at stable memory address 0 with
initial word encoding size (in bytes?) followed by contents.
(Candidish is the Motoko extension of Candid to support mutable data.)

Starting from a clean slate, we would extend this so all user-defined StableMemory is
stored at a low address, with _stable variable_ data stored just
beyond the currently used StableMemory content on canister_pre_upgrade
and canister_post_upgrade. That way the StableMemory area need not
move, with stable variables simply serialized and appended in
`canister_pre_upgrade` and deserialized and discarded in
`canister_post_upgrade`, leaving the manual StableMemory unchanged.

For backwards compatibility reasons, we can't do that.

Luckily, stable variables always require non-zero bytes to encode, so we
can devise a backwards compatible scheme for upgrading
from pre-StableMemory canisters to post-StableMemory
canisters, as follows.

During execution, abstract stable memory (StableMemory) is aligned
with IC stable memory, at address 0, for reasonable efficiency (apart
from bound checks against logical `size()`).

During upgrade, if StableMemory has zero pages, we use the existing format, writing
(non_zero) length and content of any stable variables from address 0 or leaving ic0.stable_mem()
at zero with no pages allocated (if there are no stable variables).
Otherwise, we compute the length and data of the stable variable encoding;
save the first word of StableMemory at a known offset from the end of physical memory;
write a 0x00 marker to the first word; and append length (even if zero) and
data (if any) to the end of StableMem.
The logical size of StableMemory and a version number are also written at
known offsets from the end of StableMemory.

In post_upgrade, we reverse this process to recover the size of StableMemory,
restore the displaced first word of StableMemory and deserialize any stable vars,
taking care to zero the (logically) free StableMemory occupied by any encoded stable variables
and other metadata (so that initial reads after growing beyond page `size`  always return 0).

This scheme avoids relocating most of StableMem and is constant time when
there are no stable variables.

# Details:

Stable memory layout (during execution):

*  aligned with stable-memory, with global word `size` holding logical page count (initially 0 < !size < 2^16).
*  user are responsible for allocating logical pages.
*  each load/store does a `size`-related bounds check.

During execution, stable variables aren't maintained in stable memory - they are on the Motoko heap.

Stable memory layout (between upgrades), assuming stable variable encoding `v == {fs:vs}`, a record value of record type {fs:Ts}.

NOTE: A program with no stable variables still writes an empty record value `v = {}`.

```
(case !size == 0) // hence N = 0
  [0..3] StableVariable data len
  [4..4+len-1] StableVariable data
  [4+len-1,..M-1] 0...0 // zero padding
(case !size > 0)
[0..3]  0...0
[4..N-1]  StableMemory bytes
[N..N+3]  StableVariable data len
[N+4..(N+4)+len-1] StableVariable data
[(N+4)+len..M-13] 0...0 // zero padding
[M-12..M-9] value N/64Ki = !size
[M-8..M-5] saved StableMemory bytes
[M-4..M-1]  version word

where N = !size * pagesize // logical memory size
      M = ic0.stable_size() * pagesize // physical memory size
      pagesize = 64Kb (2^16 bytes)
where (len, data) = serialize<Ts>(v,data)

```

On pre_upgrade

```ocaml
func stabilise {fs:Ts} v : value =
  let len, data = serialize<Ts>(v)
  in
  if !size == 0 then
    mem[0,..,3] := len
    mem[4,...,4+len-1] := data
  else
    let N = !size * page_size in
    // if necessary, grow mem to page including address N + 4 + len + 4 + 4 + 4
    let M = pagesize * ic0.stable_size() in
    mem[N,..,N+3] := len
    mem[N+4,..,N+4+len-1] := data
    mem[M-12..M-9] := !size
    men[M-8..M-5] := mem[0,...,3] // save StableMemory bytes 0-3
    mem[0,..,3] := 0..0 // write marker
    mem[M-4..M-1] := version
```
on post_upgrade

```ocaml
// restores StableMemory (size and memory) and deserializes any stable variables, zeroing their storage
fun destabilize {fs:Ts} : value =
  let pages = ic0.stable_size() in
  if pages == 0 then
    size := 0;
    {fs = nulls}
  else
    let marker = mem[0,..,3] in // read zero or size of stable value
    mem[0,..,3] = 0;
    let (offset, len) =
      if marker == 0x0 then
        let M = pages * pagesize in
        let ver = mem[M-4,..,M-1] in
        mem[M-4,..,M-1] := 0;
        if (ver > version) assert false
        mem[0,..,3] = mem[M-8,..,M-5]; // restore StableMemory bytes 0-3
        size := mem[M-12,..,M-9];
        mem[M-12,..,M-9] := 0;
        N = size * pagesize;
        let len = mem[N,..,N+3] in
        mem[N,..,N+3] := 0;
        assert len > 0
        assert (N+4+len-1 <= ic0.stable_size() * pagesize)
        (N+4, len)
      else
        (4, marker)
    in
    assert (0 < len <= ic0.stable_size() * pagesize)
    let v = deserialise<Ts>(offset, len) in
    mem[offset, len] := 0 // clear serialization memory
    v
```

We explicitly clear memory used by stable variables so StableMem
doesn't need to clear memory when grabbing logical pages from already
existing physical ones.


NOTE: We still need to do some work during updgrade and postupgrade,
but if stable variables and user-defined pre/post upgrade hooks are
avoided, then the work is minimal and highly unlikely to exhaust cycle
budget.

REMARK:

* An actor that has no stable variables and allocates no StableMem
  should requize no physical stable memory

* An actor that has n > 0 pages of StableMem will (unfortunately)
  require at least n+1 pages of physical memory since we need at least
  one extra bit to encode the presence or absence of stable variables
  (there is no other preserved state that could record this bit).

FURTHER CONSIDERATIONS: It would be nice if there was some way to
allow a Motoko actor (perhaps intended to upgrade from a foreign
canister with foreign stable memory format) to have unadulterated,
full-speed access to stable memory, without the protocol required by
the possibility of stable variables.  Perhaps we could special case
programs that have *no* stable variables to support this raw
semantics.  I.e. if the program declares no stable variables, we
install and upgrade by simply setting `(StableMemory.)size :=
ic0.stable_size()`, never consulting or altering physical memory and
(ideally) omitting the additional bounds checking.
