//SKIP run
//SKIP run-ir
//SKIP run-low
//INCREMENTAL-GC-ONLY

import Prim "mo:prim";

let _a = Prim.Array_init<Nat8>(1024 * 50, 0);
Prim.debugPrint(debug_show {mem = Prim.rts_memory_size() / (1024 * 1024); heap = Prim.rts_heap_size() / (1024 * 1024)});
