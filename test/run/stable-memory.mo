import Prim "mo:prim";

Prim.debugPrint(debug_show {size = Prim.stableMemorySize()});
let 0 = Prim.stableMemoryGrow(16);
let 16 = Prim.stableMemorySize();
Prim.debugPrint(debug_show {size = Prim.stableMemorySize()});

//SKIP run-low
//SKIP run
//SKIP run-ir