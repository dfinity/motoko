import Prim = "mo:⛔";
(with migration =
   func({
     f : Any // reject - lossy supertype
     }) :
   { f : Int} =
   { f = -1 }
)
persistent actor {

  var f : Int = Prim.trap("impossible");
  assert false

}
