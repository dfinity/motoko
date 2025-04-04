import Prim = "mo:⛔";
(with migration =
   func({
     f : Int // accept
     }) :
   { } =
   { }
)
persistent actor {
  var g : Nat = 0;
  Prim.debugPrint "version4";
}
