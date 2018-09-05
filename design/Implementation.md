# ActorScript Implementation Strategy

## Heap

* Uniform representation with 32 bit word size.

* Use pointer tagging in LSB;.
  - 1 for pointers, 0 for scalars.
  - Pointers are real address minus 1; corrected by offset immediate in load/store instructions.

* Q: Allocation and GC strategies?


## Numbers

* Nat and Int compile to heap-allocated big nums; unboxed for small numbers <= 31 bit.

* Word8/16 compile to unboxed scalars; Word32/64 are boxed.
  - May unbox locally.


## Objects

* Compile to heap-allocated lookup table over field name hashes.
  - Either as hash table or ordered list.

* Records of closures and regular public fields; private fields are closed over.

* Field names are hashed; hashes are required to be unique per type.


## Functions

* Compile to heap-allocated closures as usual.

* Definitions lifted to the surrounding actor.

* Closed-over locals need indirection through heap if mutable or if definedness cannot be proved.


## Actor Objects

* Compile to immediately instantiated modules.

* Private field become regular globals or functions.

* Public methods become exported functions.
  - In general, have to generate wrapper functions to forward external calls to pre-existing local closure.

* Instantiate via hypervisor; build record of functions forwarding to extracted funcrefs.

* Closed-over definitions must be turned into imports.
  - Types directly mapping to Wasm types become (immutable) global imports.
  - Q: What about heap-allocated types? Serialise?
  - Q: What about functions?
  - Q: Disallow closing over mutable definitions?
  - Q: What about definedness checks?

* Closure imports are pre-bound via hypervisor interface.


## Actor Classes

* Constructor compiles to function instantiating the actor object.


## Async

TODO


## Subtyping

* All subtyping is non-coercive and thus a no-op.

* Q: Also have first-order coercions?


## Serialisation

TODO


# Hypervisor Extensions needed

* Closures as funcrefs?

* Binding imports for modulerefs

* Efficiently extract all exports from an actor
  - as sequence of reference in an elembuf?
