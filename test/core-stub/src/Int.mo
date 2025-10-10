/// Stub for Int.

import Types "Types";

module {
  public type Self = Int;

  public func compare(x : Int, y : Int) : Types.Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };
}
