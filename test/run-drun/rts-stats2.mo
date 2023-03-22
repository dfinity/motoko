import Prim "mo:⛔";
actor a {
  // Object table and remembered used by the incremental GC.
  // Remembered set used by the generational GC.
  let minimumSize = 23_000_000;
  let arrayLength = 1000_000;
  let reserve = 3_000;

  public func foo() {
    ignore(Prim.Array_init<()>(arrayLength, ()));
  };
  public func check_A() {
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show Prim.rts_reclaimed());
    assert (Prim.rts_reclaimed() > 4 * arrayLength);
    // Generational GC has additional remembered set that is discarded on each GC run
    // Debug mode for generational GC also produces additional memory snapshots for sanity checks
    assert (Prim.rts_reclaimed() < 4 * arrayLength + reserve);

    Prim.debugPrint("Ignore Diff: Live size: " # debug_show Prim.rts_max_live_size());
    assert (Prim.rts_max_live_size() < minimumSize);
  };
  flexible var v : [var ()] = [var];
  public func bar() {
    v := Prim.Array_init<()>(arrayLength, ());
  };
  public func check_B() {
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show Prim.rts_reclaimed());
    assert (Prim.rts_reclaimed() > 4 * arrayLength);
    // Generational GC has additional remembered set that is discarded on each GC run
    // Debug mode for generational GC also produces additional memory snapshots for sanity checks
    assert (Prim.rts_reclaimed() < 4 * arrayLength + reserve);

    Prim.debugPrint("Ignore Diff: Live size: " # debug_show Prim.rts_max_live_size());
    assert (Prim.rts_max_live_size() > 4 * arrayLength);
    assert (Prim.rts_max_live_size() < minimumSize + 4 * arrayLength);
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
