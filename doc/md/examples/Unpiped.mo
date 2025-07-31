import Iter "mo:core/Iter";
import List "mo:core/List";
import Nat "mo:core/Nat";

{ multiples =
   List.filter<Nat>(
     Iter.toList(Nat.range(0, 10)),
     func n { n % 3 == 0 }) };
