//MOC-FLAG --persistent
import Prim "mo:⛔";
actor {

  var x : {#x} = Prim.trap "ohoh";
  var y : {#y} = #y;
  Prim.debugPrint(debug_show {x;y});

}
