import Prim "mo:⛔";
actor a {

  public func ping() : async () {
    Prim.debugPrint("ping! " # debug_show Prim.rts_callback_table_count());
  };

  public func go() : async () {
    Prim.debugPrint("go 1: " # debug_show Prim.rts_callback_table_count());
    let a1 = ping();
    Prim.debugPrint("go 1: " # debug_show Prim.rts_callback_table_count());
    let a2 = ping();
    Prim.debugPrint("go 1: " # debug_show Prim.rts_callback_table_count());
    let a3 = ping();
    Prim.debugPrint("go 1: " # debug_show Prim.rts_callback_table_count());
    let a4 = ping();

    Prim.debugPrint("go 1: " # debug_show Prim.rts_callback_table_count());
    await a1;
    Prim.debugPrint("go 2: " # debug_show Prim.rts_callback_table_count());
    await a2;
    Prim.debugPrint("go 3: " # debug_show Prim.rts_callback_table_count());
    await a3;
    Prim.debugPrint("go 4: " # debug_show Prim.rts_callback_table_count());
    await a4;
    Prim.debugPrint("go 5: " # debug_show Prim.rts_callback_table_count());
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
