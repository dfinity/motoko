//MOC-FLAG --default-persistent-actors
import Prim "mo:⛔";
actor {

  var x : {#x} = Prim.trap "ohoh";
  Prim.debugPrint(debug_show {x});

}
