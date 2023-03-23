import Prim "mo:⛔";
actor a {
  // Object table and remembered used by the incremental GC.
  // Remembered set used by the generational GC.
  let minimumSize = 23_000_000;
  let arrayLength = 1_000_000;
  let rememberedSetReserve = 2_100_000;

  var lastReclaimed = +0;

  public func foo() {
    lastReclaimed := Prim.rts_reclaimed();
    ignore(Prim.Array_init<()>(arrayLength, ()));
  };
  public func check_A() {
    var reclaimed = Prim.rts_reclaimed() - lastReclaimed;
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show reclaimed);
    assert (reclaimed > 4 * arrayLength);

    Prim.debugPrint("Ignore Diff: Live size: " # debug_show Prim.rts_max_live_size());
    assert (Prim.rts_max_live_size() < minimumSize);
  };
  flexible var v : [var ()] = [var];
  public func bar() {
    lastReclaimed := Prim.rts_reclaimed();
    v := Prim.Array_init<()>(arrayLength, ());
  };
  public func check_B() {
    var reclaimed = Prim.rts_reclaimed() - lastReclaimed;
    lastReclaimed += reclaimed;
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show reclaimed);
    // Remembered set reserve
    assert (reclaimed < rememberedSetReserve);

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
