// This file defines a single actor (object) named Counter.
//
// Internally, Counter holds a natural number, its current count.

// Public Spec of Counter actor
// ----------------------------

//
// To describe Counter's external behavior, we refer to
// its internal count, a natural number, as `cell`:
//
// - the `inc` message increments `cell`, with unit response value
//
// - the `read` message requests the value held in `cell` in response
//

// An implementation of the Counter actor
// ----------------------------------------------------------------------
//
// See below.  
// Uses fewer than 10 lines of ActorScript, and many are just closing braces.
//
// Aside: to make checking the spec above as easy as possible, we
// intentionally reuse the name `cell` mentioned there for the single
// mutable variable in the implementation below; we didn't have to do
// this, of course, since the spec is technically independent of these
// implementation details:

actor Counter {
  
  // our single variable, `cell`, holds the current count
  var cell : Nat = 0;

  // increment `cell`, with a unit-valued (asynchronous) response
  public func inc() : async () {
    cell += 1;
  };

  // request the value held in `cell` in (asynchronous) response
  public func read () : async Nat {
    cell
  };

}
