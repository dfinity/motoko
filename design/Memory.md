# Memory Management and Persistence in Wasm/IC/Motoko

## Preliminaries

### Wasm

#### Values

Wasm has two kinds of data (assuming the upcoming reference type proposal):

* *Numerics* (int32/64, float32/64): *transparent* data; bit pattern observable; can be stored in memory
* *References* (anyref, funcref, ...): *opaque* data; representation not exposed, usually implemented as pointers; can be stored in tables but not memory

#### State

Wasm currently has 3 forms of mutable state, all of which can be ex/imported from a module as concrete entities:

* *Globals*: can each store a single value; can be mutated (if defined as mutable)
* *Memory*: an array of raw bytes; can be mutated and grown; can store numeric values
* *Tables*: an array of references; can be mutated and grown; can store reference values

A reference can also point to *host objects* provided by the embedder, which could introduce additional forms of state at the embedder's discretion.

#### Further Evolution

In the future (with the GC proposal), Wasm will have a 4th form of mutable state:

* *Heap*: memory area used for allocating GCed structures; distinct from Memory; controlled by VM

The Heap is *not* an explicit entity that can be im/exported, only individual references to structures on the heap can be passed.

Note: It is highly likely that most languages implemented on Wasm will eventually use Wasm GC.
Various implementers are currently waiting for it to become available before they start porting their language to Wasm.

### Internet Computer (IC)

#### API Types

The Hypervisor API introduces a set of Wasm host reference types to represent actors, functions, and their arguments:

* *Modules*: a reference to a compiled but not yet instantiated actor (Wasm module); can be linked and instantiated multiple times
* *Actors*: a reference to an instantiated and active actor
* *Functions*: a reference to an exported actor method; invoking it initiates an asynchronous message send
* *Data buffers*: a reference to an immutable array of bytes; used to pass binary data or aggregates of numeric values along messages
* *Element buffers*: a reference to an immutable array of references; used to pass aggregates of references along messages

All references are *sharable*, i.e., can be passed between actors as message arguments.
Other than actors, all reference types must be pure (immutable and without identity) to prevent shared state and allow transparent copying by the implementation.
Element buffers can encode arbitrary object trees.

Once Wasm GC is available, some of these types (esp. buffers) could be replaced by proper Wasm types.


## Language Implementation

### Representing Data Structures

There are 3 possible ways of representing structured data in Wasm/IC.

#### Using Wasm Memory

All data structures are laid out and managed in Memory by the compiler and the language runtime.
References are stored via indirections through a Table.

   Pros:
   1. local data access maximally efficient
   2. may ease transparent persistence (see below)

   Cons:
   1. message arguments require de/serialisation into IC buffers on both ends (in addition to the de/serialisation steps already performed by IC)
   2. each actor must ship its own instance of a GC (for both memory and table) and de/serialisation code
   3. all references require an indirection
   4. more implementation effort

#### Using IC API

All data structures are mapped to element and data buffers created through the API.

   Pros:
   1. quickest to implement and get off the ground
   2. no extra de/serialisation step for message sends
   3. easy transition path to Wasm Heap

   Cons:
   1. local access to data involves system call overhead
   2. requires adding mutable buffer types to IC API
   3. mixed numeric/reference structures must be split into data and element buffers
   4. IC-only solution unlikely to be adopted by other languages

#### Using Wasm Heap

All data structures are represented as Wasm GCed objects.

   Pros:
   1. local access should be as fast as Memory
   2. GC is handled by VM
   3. no extra de/serialisation step for message args (provided IC API can handle Wasm structs)
   4. can freely mix numerics and references
   5. this is likely the route most future languages compiling to Wasm will take

   Cons:
   1. Wasm GC is 1-2 years out
   2. unclear how to implement transparent persistence (see below)


## Persistence

### Persistence models

There are at least 3 general models for providing persistence.

#### *Explicit* persistence

IC API provides explicit system calls to manage persistent data.
Wasm state is volatile; each message received runs in a fresh instance of the actor's module.

   Pros:
   1. easy and efficient to implement
   2. apps have maximal control over persistent data and its layout

   Cons:
   1. bifurcation of state space
   2. programs need to load/store and de/serialise persistent data to/from local state

#### *Transparent* persistence

All Wasm state is implicitly made persistent.
Conceptually, each message received runs in the same instance of the actor's module.

   Pros:
   1. "perfect" model of infinitely running program
   2. programmers need to "think" less

   Cons:
   1. hard to implement efficiently without knowing neither language nor application
   2. can easily lead to space leaks or inefficiencies if programmers aren't careful

#### *Hybrid* persistence
Wasm state entities can be marked as persistent selectively.
Conceptually, each message received runs in the same instance of the actor's module,
but Wasm is extended with some notion of volatile state and reinitialisation.

   Pros:
   1. compromise between other two models

   Cons:
   1. compromise between other two models
   2. creates dangling references between bifurcated state parts
   3. incoherent with Wasm semantics (segments, start function)

### Implementing Transparent persistence

#### *High-level* implementation of persistence

Hypervisor walks data graph (wherever it lives), turns it into merkle tree.

   Pros:
   1. agnostic to implementation details of the engine
   2. agnostic to GC (or subsumes GC)

   Cons:
   1. requires knowledge of and access to data graph
   2. deep mutations result in deep changes in merkle tree (mutation cost is logarithmic in depth)
   3. unclear how to detect changes efficiently

#### *Low-level* implementation of persistence

Hypervisor provides memory to Wasm engine, detects dirty pages; could be memory-mapped files.

   Pros:
   1. agnostic to language and data graph
   2. fast when mutation patterns have good locality
   3. can potentially offload much of the implementation to existing hardware/OS/library mechanisms

   Cons:
   1. bad interaction with language-internal GC (mutates large portions of the memory at once)
   2. does not extend to tables (contain position-dependent physical pointers)
   3. no obvious migration path to Wasm GC
   4. dependent on VM specifics (and internals?)

#### *Selectable* implementation of persistence

Provide both previous options, possibly in a mutually exclusive fashion.

   Pros:
   1. choice for implementers

   Cons:
   1. maximal complexity for platform
