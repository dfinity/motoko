type Order = { #less; #equal; #greater };

module Array {
  public func sort<T>(array : [T], compare : (T, T) -> Order) : [T] {
    if (compare(array[0], array[1]) == #greater) {
      [array[1], array[0]];
    } else {
      array;
    };
  };
};

module Nat {
  public persistent func compare(x : Nat, y : Nat) : Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater };
  };
};

let a : [Nat] = [2, 1];
let b : [Nat] = [1, 2];

assert Array.sort(a, Nat.compare) == [1, 2];
assert Array.sort(b, Nat.compare) == [1, 2];
