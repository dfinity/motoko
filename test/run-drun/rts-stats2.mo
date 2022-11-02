import Prim "mo:⛔";
actor a {
  public func foo() {
    ignore(Prim.Array_init<()>(2500, ()));
  };
  public func check_A() {
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show Prim.rts_reclaimed());
    // 10116 at some point
    // assert (Prim.rts_reclaimed() > 10000);
    // assert (Prim.rts_reclaimed() < 11000);

    Prim.debugPrint("Ignore Diff: Live size: " # debug_show Prim.rts_max_live_size());
    // 8 at some point
    // assert (Prim.rts_max_live_size() < 100);
  };
  flexible var v : [var ()] = [var];
  public func bar() {
    v := Prim.Array_init<()>(2500, ());
  };
  public func check_B() {
    Prim.debugPrint("Ignore Diff: Reclaimed: " # debug_show Prim.rts_reclaimed());
    // 10_716 at some point
    // assert (Prim.rts_reclaimed() > 10000);
    // assert (Prim.rts_reclaimed() < 12000);

    Prim.debugPrint("Ignore Diff: Live size: " # debug_show Prim.rts_max_live_size());
    // 10_008 at some point
    // assert (Prim.rts_max_live_size() > 10000);
    // assert (Prim.rts_max_live_size() < 11000);
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
