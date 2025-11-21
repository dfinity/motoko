// test wild-card syntax works too
// (contrived example)
import {debugPrint} "mo:prim";

module Nat {
  public func nat(n : Nat) : Text { debug_show n };
};


module Int {
  public func int(i : Int) : Text { debug_show i };
};


module Array {

  public func toText<T>(as : [T], implicit toText : (_ : T -> Text)) : Text {
     var t = "";
     for (a in as.vals()) {
       t := t # (toText(a));
     };
     t
  }

};


module Pair {

  public func toText<T,U>(
    implicit toTextT : (_ : T -> Text),
    implicit toTextU : (_ : U -> Text),
    p : (T, U))
    : Text {
      "(" # toTextT(p.0) # "," # toTextU(p.1) # ")"
    };

};


func test () {

  debugPrint(Array.toText([1,2,3], Nat.nat)); // explicit arguments
  debugPrint(Array.toText([1,2,3])); // implicit arguments

  debugPrint(Pair.toText(Nat.nat, Int.int, (1,2)));
  debugPrint(let _ = Pair.toText((1,2))); // implicit arguments
  debugPrint(let _ = Pair.toText((1,2))); // implicit arguments

};

test();
