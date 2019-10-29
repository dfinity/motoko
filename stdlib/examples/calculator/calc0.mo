// Single-cell calculuator, first version (version 0).

// this simple actor class defines one calculator instruction per
// public entry point (add, sub, mul, div).

// Note: `div` returns an optional Nat, and it avoids doing a division by
// zero; we do not want to divide by zero and crash the calculator
// service.  Later, we can show how to do `try`-`catch` block for this,
// which gives another approach (but probably to be avoided, as we do here?).

actor class Calc(init:Nat) {
  var cell : Nat = init;

  public func add(n:Nat) : async Nat { cell += n; cell };
  public func sub(n:Nat) : async Nat { cell += n; cell };
  public func mul(n:Nat) : async Nat { cell += n; cell };
  public func div(n:Nat) : async ?Nat {
    if ( n == 0 ) {
      // null encodes div-by-zero error
      return null
    } else {
      cell /= n; ?cell
    }
  };
 };
