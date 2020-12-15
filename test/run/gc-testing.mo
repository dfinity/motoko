//MOC-FLAG --gc-testing
//SKIP run
//SKIP run-ir
//SKIP run-low

import Prim "mo:prim";

var i = 0;

Prim.debugPrint("main program running... i = " # debug_show i);

func post_gc() : Bool {
  Prim.debugPrint("post gc code running... i = " # debug_show i);
  Prim.debugPrint("Ignore Diff: Heap size: " # debug_show Prim.rts_heap_size());
  Prim.debugPrint("Ignore Diff: Total allocation: " # debug_show Prim.rts_total_allocation());
  Prim.debugPrint("Ignore Diff: Max live size: " # debug_show Prim.rts_max_live_size());
  Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show Prim.rts_reclaimed());
  i += 1;
  return (i <= 5)
}

