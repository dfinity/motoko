import Prim "mo:prim";

Prim.debugPrint(debug_show {size = Prim.stableMemorySize()});
let 0 = Prim.stableMemoryGrow(16);
let 16 = Prim.stableMemorySize();
Prim.debugPrint(debug_show {size = Prim.stableMemorySize()});
let 0 = Prim.stableMemoryLoadNat8(0);
Prim.debugPrint(debug_show {read = Prim.stableMemoryLoadNat8(0)});
Prim.stableMemoryStoreNat8(0,66);
Prim.debugPrint(debug_show {read = Prim.stableMemoryLoadNat8(0)});
let 66 = Prim.stableMemoryLoadNat8(0);
Prim.debugPrint(debug_show {read = Prim.stableMemoryLoadNat8(0)});

//SKIP run-low
//SKIP run
//SKIP run-ir