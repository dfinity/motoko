import Prim "mo:⛔";
actor a {
  let length = 1024 * 1024; 
  public func foo(): async() {
    await async { // extra GC increment for incremental GC
      ignore(Prim.Array_init<()>(length, ())); 
    };
    await async(); // extra GC increment for incremental GC
  };
  public func check_A() {
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show Prim.rts_reclaimed());
    assert (Prim.rts_reclaimed() > 4 * length);
    // Generational GC has additional remembered set that is discarded on each GC run
    // Debug mode for generational GC also produces additional memory snapshots for sanity checks
    assert (Prim.rts_reclaimed() < 5 * length);

    Prim.debugPrint("Ignore Diff: Live size: " # debug_show Prim.rts_max_live_size());
    assert (Prim.rts_max_live_size() < 250_000);
  };
  flexible var v : [var ()] = [var];
  public func bar(): async() {
    await async { // extra GC increment for incremental GC
      v := Prim.Array_init<()>(length, ()); // larger amount to trigger incremental GC
    };
    await async(); // extra GC increment for incremental GC
  };
  public func check_B() {
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show Prim.rts_reclaimed());
    assert (Prim.rts_reclaimed() > 4 * length);
    // Generational GC has additional remembered set that is discarded on each GC run
    // Debug mode for generational GC also produces additional memory snapshots for sanity checks
    assert (Prim.rts_reclaimed() < 8 * length);

    Prim.debugPrint("Ignore Diff: Live size: " # debug_show Prim.rts_max_live_size());
    assert (Prim.rts_max_live_size() > 4 * length);
    assert (Prim.rts_max_live_size() < 5 * length);
  };
}
// no point running these in the interpreter
//SKIP run
//SKIP run-low
//SKIP run-ir

//CALL ingress foo "DIDL\x00\x00"
//CALL ingress check_A "DIDL\x00\x00"
//CALL ingress check_A "DIDL\x00\x00"
//CALL ingress bar "DIDL\x00\x00"
//CALL ingress check_B "DIDL\x00\x00"
//CALL ingress check_B "DIDL\x00\x00"
