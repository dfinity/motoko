// test allocation beyond the 32-bit address space
import P "mo:⛔";
do {

  // 5 GB allocations
  var c = 5;

  while(c > 0) {
    let a : [var Nat8] = P.Array_init<Nat8>(1024*1024*1024/4, 0xFF);
    c -= 1;
  };

  P.debugPrint("Memory: " # debug_show(P.rts_memory_size()));
  P.debugPrint("Heap: " # debug_show(P.rts_heap_size()));
}

//SKIP run
//SKIP run-low
//SKIP run-ir


