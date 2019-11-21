import P "prelude.mo";
import B "buf.mo";

module {
/**
Array Buffers
===================

Growable buffers, implemented as arrays.

See [buffer documentation](buf.mo) for more.

*/

// re-export types and use them below.
public type ArrayBuf<X> = B.ArrayBuf<X>;
public type IndexedBuf<X> = B.IndexedBuf<X>;

/*
Create an IndexedBuf<X>, represented internally by an mutable array.

The argument `initCapacity` gives the initial capacity.  Under the
interface, the mutable array grows by doubling when this initial
capacity is exhausted.
*/
public class Buf<X> (initCapacity : Nat) : IndexedBuf<X> {
  var count : Nat = 0;
  var elems : [var X] = [var]; // initially empty; allocated upon first `add`

  public func add(elem : X) {
    if (count == elems.len()) {
      let size =
        if (count == 0)
          initCapacity
        else
          2 * elems.len();
      let elems2 = Array_init<X>(size, elem);
      for (i in elems.keys()) {
        elems2[i] := elems[i];
      };
      elems := elems2;
    };
    elems[count] := elem;
    count += 1;
  };

  public func addBuf(b:B.Buf<X>) {
    for (x in b.iter()) { add(x) }
  };

  public func len() : Nat =
    count;

  public func clear() =
    count := 0;

  public func clone() : IndexedBuf<X> {
    let c = Buf<X>(initCapacity);
    for (elem in iter()) { c.add(elem) };
    c
  };

  public func iter() : Iter<X> = object {
    var pos = 0;
    public func next() : ?X {
      if (pos == count) { null } else {
        let elem = ?elems[pos];
        pos += 1;
        elem
      }
    }
  };

  public func array() : [X] =
    // immutable clone of array
    Array_tabulate<X>(
      elems.len(),
      func(x: Nat): X { elems[x] }
    );

  public func mutArray() : [var X] = {
    if (count == 0) { [var] } else {
      let a = Array_init<X>(count, elems[0]);
      for (i in elems.keys()) {
        a[i] := elems[i]
      };
      a
    }
  };

  public func arrayBuf() : ArrayBuf<X> {
    // overcome common field names between source/target objects:
    let e = elems;
    let c = count;
    { var elems = e;
      var count = c }
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

  public func getEnd(offset : Nat) : X {
    elems[count - offset]
  };

  public func getEndOpt(offset : Nat) : ?X {
    if (count > offset) { ?elems[count - offset] }
    else { null }
  };

  public func setEnd(offset : Nat, elem : X) {
    elems[count - offset] := elem;
  };
};

}
