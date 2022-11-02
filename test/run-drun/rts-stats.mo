import Prim "mo:⛔";
let s0 = Prim.rts_heap_size();
let a0 = Prim.rts_total_allocation();
ignore(Prim.Array_init<()>(2500, ()));
let s1 = Prim.rts_heap_size();
let a1 = Prim.rts_total_allocation();

// the following are too likey to change to be included in the test output
// Prim.debugPrint("Size and allocation before: " # debug_show (s0, a0));
// Prim.debugPrint("Size and allocation after:  " # debug_show (s1, a1));

// this should be rather stable unless the array representation changes
Prim.debugPrint("Size and allocation delta:  " # debug_show (s1-s0 : Nat, a1-a0 : Nat));
assert (+s1-s0 == 20024);
assert (+a1-a0 == 20024);

assert(Prim.rts_memory_size() > Prim.rts_heap_size());

// no point running these in the interpreter
//SKIP run
//SKIP run-low
//SKIP run-ir
