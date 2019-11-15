import P "prelude.mo";
import Buf "buf.mo";
import Option "option.mo";

module {
/**
Array Buffers
===================

Growable buffers, implemented as arrays.

See [buffer documentation](buf.mo) for more.

*/

public type ArrayBuf<X> = {
  array: [var ?X];
  var count: Nat;
};

/**
#### Terminology:

The buffer abstraction says `len` for what we call `count` here internally.

We do this because `array.len()` gives the _capacity_ of the buffer's
current array, but not its current (growable) "length" within that
array (which again, we call "count" to avoid further confusion here).

*/

public type InitArg<X> = {
  // no initial info
  #none;
  // take ownership of an existing representation
  #arrayBuf: ArrayBuf<X>;
  // initially-empty, with specified capacity
  #capacity: Nat;
  // initialize with given elements, and additional capacity
  #array: [X];
};

// Create a Buf<X> instance, represented as an ArrayBuf<X> instance
public class Make<X> (init:InitArg<X>) : Buf.Buf<X> {
  var buf : ArrayBuf<X> = switch init
  {
  case (#arrayBuf(buf)) buf;
  case (#none)
  { // 0 is "most conservative" on initial size:
    array = Array_init<?X>(0, null);
    var count = 0;
  };
  case (#capacity(initCapacity))
  {
    array = Array_init<?X>(initCapacity, null);
    var count = 0;
  };
  case (#array(array_))
  {
    // allocate new buffer,
    let newBuf = {
      array = Array_init<?X>(array_.len() * 2, null);
      var count = array_.len()
    };
    // initialize via immutable array arg
    for (i in array_.keys()) {
      newBuf.array[i] := ?(array_[i])
    };
    newBuf
  };
  };

  public func add(x:X) {
    if (buf.count < buf.array.len()) {
      Option.assertNullAny(buf.array[buf.count]);
      buf.array[buf.count] := ?x;
      buf.count += 1;
    } else {
      // todo: grow
      P.nyi()
    }
  };

  public func addBuf(b:Buf.Buf<X>) {
    for (x in b.iter()) { add(x) }
  };

  public func len() : Nat =
    buf.count;

  public func clear() {
    // clear away elements, but do not reallocate
    for (x in buf.array.keys()) {
      buf.array[x] := null
    };
    buf.count := 0
  };

  public func clone() : Buf.Buf<X> {
    /** Blocked: See issue 871 */
    P.nyi()
  };

  public func iter() : Iter<X> =
    // todo
    P.nyi();

  public func array() : [X] =
    // immutable clone of array
    Array_tabulate<X>(
      buf.array.len(),
      func(x: Nat): X {
    Option.unwrap<X>(buf.array[x])
  });

  public func mutArray() : [var X] = {
    /** Blocked: See issue 871 */
    P.nyi()
  };
};

}
