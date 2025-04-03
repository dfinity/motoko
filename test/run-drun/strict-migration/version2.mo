import Prim = "mo:⛔";

(with migration =
   func({
     f : Nat // ok - exact type
     }) :
   { f : Int} =
   { f = f }
)
persistent actor {
  var f : Int = Prim.trap("impossible");
  Prim.debugPrint("version2");
}
