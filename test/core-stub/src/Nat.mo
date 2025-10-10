/// Stub for Nat.

import Types "Types";

module {
  public func compare(x : Nat, y : Nat) : Types.Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };
}