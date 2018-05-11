# ActorScript Implementation Strategy

## Basic Principles

* All heap allocation via Primea

* Actor classes and objects compile to modules and module instances
  - closing over environment via bound imports

* Regular objects compile to label-sorted elembufs
  - record of closures closing over private fields
  - numeric fields collected in databuf
  - mutable fields via indirections

* Subtyping is coercive

* Closures compile to closure funcrefs
  - locals that are closed over compile to databufs/elembufs


## Primea Extensions

* Mutable elembufs and databufs, cannot be shared

* Load and store functions for buffers

* Closures as funcrefs

* Binding imports for modulerefs

* Efficiently extract all exports from an actor
  - as sequence of reference in an elembuf?

* Intrefs?
