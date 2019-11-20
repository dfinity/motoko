module {
/**
Buffers
===================

This module defines common types for buffers that grow, with a general element type.

Implementations of these types are/will be defined elsewhere.

### Why?

Motoko applications expose interfaces that use arrays of general (user-defined)
elements to represent sets, sequences and maps of application-specific
elements.

To create these arrays, and to consume them with ergonomic (imperative) code, and
low API friction, developers need _buffers that grow_.

The `Array` module focuses on Motoko's builtin arrays, whose size is
each fixed.  They do not permit general growth/appending, which is the
focus here.

### Define `Buf<X>` object type

A "buffer" is a mutable sequence that grows, either one element at a time, or one (second) buffer at time.

The only way to access elements is with iteration, or by producing an array (either mutable or immutable).

We capture the most basic buffer abstraction with the following `Buf<X>` object type:
*/

public type Buf<X> = {
  // intro forms: addElm and addBuf
  add: X -> ();
  addBuf: Buf<X> -> ();

  // utilities:
  len: () -> Nat;
  clear: () -> ();
  clone: () -> Buf<X>; // copies do not interfere across future mutations

  // elim forms for accessing elements:
  iter: () -> Iter<X>;
  array: () -> [X];
  mutArray: () -> [var X]; // bikeshed decision: `varArray` instead?
  arrayBuf: () -> ArrayBuf<X>; // can expose _spare capacity_, if any
};

/*
 A type that can represent and implement Buf<X>; See [`ArrayBuf`](arrayBuf.mo) for details.

 invariants:
 1. the first `count` elements are "filled" in the `elems` array;
 2. any remaining slots in `elems` contain a default value of type `X`.
*/
public type ArrayBuf<X> = {
  var elems: [var X];
  var count: Nat;
};

/**

### Define `IndexedBuf<X>` object type

A more general version of `Buf` that permits **indexed-based random
access** of its elements, independent of iteration or producing an
auxiliary array.

##### Addressing accesses and updates

By default, to _address_ an element for access, we use an offset from
the start of the buffer, beginning at zero.

Additionally, the abstraction provides access by a (zero-based) offset
from the end (via `getEnd` and `setEnd`).

*/

public type IndexedBuf<X> = {
  // intro forms: add and addBuf
  add: X -> ();
  addBuf: Buf<X> -> ();

  // utilities:
  len: () -> Nat;
  clear: () -> ();
  clone: () -> IndexedBuf<X>; // copies do not interfere across future mutations

  // elim forms for accessing elements:
  iter: () -> Iter<X>;
  array: () -> [X];
  mutArray: () -> [var X];
  arrayBuf: () -> ArrayBuf<X>;

  // get/set via an offset from the start, beginning at zero.
  get : Nat -> X; // out-of-bounds access traps (as with ordinary arrays)
  getOpt : Nat -> ?X; // `null` means "out of bounds"
  set : (Nat, X) -> ();

  // get/set via an offset from the end, beginning at zero.
  getEnd : Nat -> X; // out-of-bounds access traps (as with ordinary arrays)
  getEndOpt : Nat -> ?X; // `null` means "out of bounds"
  setEnd : (Nat, X) -> ();
};

// assert that two types are related by subtyping, without having instances:
func _assertSubtypes<X>(ramBuf:IndexedBuf<X>) : Buf<X> = ramBuf;
// (alternatively, we could have a way to "append" object types, as with `+` on Rust traits.)

}
