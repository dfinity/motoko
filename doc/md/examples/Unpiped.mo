import Iter "mo:base/Iter";
import List "mo:base/List";

{ multiples =
   List.filter<Nat>(
     Iter.toList(Iter.range(0, 10)),
     func n { n % 3 == 0 }) };
