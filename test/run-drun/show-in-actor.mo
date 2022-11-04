import Prim "mo:⛔";
Prim.debugPrint (debug_show true);
let _ = actor {
  Prim.debugPrint (debug_show false);
  Prim.debugPrint (debug_show 1);
};
()

// non-closed actors not allowed
//SKIP comp
//SKIP comp-ref
