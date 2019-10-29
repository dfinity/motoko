// Single-cell calculuator, second version (version 1).

// This actor class defines a _datatype_ (type `Instr`)
// for encoding instruction requests as data values.

// (Compare to version 0, which is simpler).

// the `eval` function evaluates the instruction by updating the
// single variable `cell`, which holds the current value of the
// (single-cell) calculuator.

type Instr = {
  #add : Nat;
  #sub : Nat;
  #mul : Nat;
  #div : Nat;
};

actor class Calc(init:Nat) {
  var cell : Nat = init;

  public func eval(instr:Instr) : async ?Nat {
    switch(instr) {
      case (#add n) { cell += n };
      case (#sub n) { cell -= n };
      case (#mul n) { cell *= n };
      case (#div n) {
        if ( n == 0 ) {
          // null encodes div-by-zero error
          return null
        } else {
          cell /= n
        }
      };
    };
    // success: non-null result via `?`
    return (?cell)
  };
};