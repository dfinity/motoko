import Prim = "mo:⛔";

(with migration =
   func({
     }) :
   { } =
   { }
)
persistent actor {
  var g : Nat = 0; // ok, exact type
  Prim.debugPrint "version6"
}
