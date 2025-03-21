import Prim = "mo:⛔";
(with migration =
   func({
     f:Int // reject - supertype
     }) :
   { f : Int} =
   { f = f }
)
persistent actor {

  var f : Int = loop {};
  assert false

}
