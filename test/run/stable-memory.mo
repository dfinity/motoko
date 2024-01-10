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

/* regions */

let r1 = Prim.regionNew();
let 0 = Prim.regionGrow(r1, 16);
let 16 = Prim.regionSize(r1);
Prim.debugPrint(debug_show {size = Prim.regionSize(r1)});
let 0 = Prim.regionLoadNat8(r1, 0);
Prim.debugPrint(debug_show {read = Prim.regionLoadNat8(r1, 0)});
Prim.regionStoreNat8(r1, 0, 66);
Prim.debugPrint(debug_show {read = Prim.regionLoadNat8(r1, 0)});
let 66 = Prim.regionLoadNat8(r1, 0);
Prim.debugPrint(debug_show {read = Prim.regionLoadNat8(r1, 0)});


//SKIP run-low
//SKIP run
//SKIP run-ir
