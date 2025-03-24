import Iter "mo:base/Iter";
import List "mo:base/List";

Iter.range(0, 10) |>
  Iter.toList _ |>
    List.filter<Nat>(_, func n { n % 3 == 0 }) |>
      { multiples = _ };
