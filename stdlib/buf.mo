import P "prelude.mo";
import I "iter.mo";
import A "array.mo";

module {

/**

Buffers
===================

This module defines buffers that grow, with a general element type.

### Why?

Motoko applications expose interfaces that use fixed-size arrays of
general (user-defined) elements to represent sets, sequences and maps
of application-specific elements.

The `Array` module focuses on Motoko's builtin arrays, whose size is
each fixed.  They do not permit general growth/appending, which is the
focus here.

To create these arrays, and to consume them with ergonomic (imperative) code, and
low API friction, developers need _buffers that grow_.

### Define `Buf<X>` object type

A "buffer" is a mutable sequence that grows, either one element at a
time, or one (second) buffer at time.

*/


/*
Create a buffer represented internally by an mutable array.

The argument `initCapacity` gives the initial capacity.  Under the
interface, the mutable array grows by doubling when this initial
capacity is exhausted.
*/
public class Buf<X> (initCapacity : Nat) {
  var count : Nat = 0;
  var elems : [var X] = [var]; // initially empty; allocated upon first `add`

  public func add(elem : X) {
    if (count == elems.len()) {
      let size =
        if (count == 0)
          initCapacity
        else
          2 * elems.len();
      let elems2 = A.init<X>(size, elem);
      for (i in elems.keys()) {
        elems2[i] := elems[i];
      };
      elems := elems2;
    };
    elems[count] := elem;
    count += 1;
  };

  public func append(b:Buf<X>) {
    let i = b.iter();
    loop {
      switch (i.next()) {
      case null return;
      case (?x) add(x);
      };
    };
  };

  public func len() : Nat =
    count;

  public func clear() =
    count := 0;

  public func clone() : Buf<X> {
    let c = Buf<X>(initCapacity);
    for (i in elems.keys()) {
      c.add(elems[i])
    };
    c
  };

  public func iter() : I.Iter<X> = object {
    var pos = 0;
    public func next() : ?X {
      if (pos == count) { null } else {
        let elem = ?elems[pos];
        pos += 1;
        elem
      }
    }
  };

  public func toArray() : [X] =
    // immutable clone of array
    A.tabulate<X>(
      elems.len(),
      func(x: Nat): X { elems[x] }
    );

  public func toVarArray() : [var X] = {
    if (count == 0) { [var] } else {
      let a = A.init<X>(count, elems[0]);
      for (i in elems.keys()) {
        a[i] := elems[i]
      };
      a
    }
  };

  public func get(offset : Nat) : X {
    elems[offset]
  };

  public func getOpt(offset : Nat) : ?X {
    if (offset < count) {
      ?elems[offset]
    }
    else {
      null
    }
  };

  public func set(offset : Nat, elem : X) {
    elems[offset] := elem;
  };
};

}
