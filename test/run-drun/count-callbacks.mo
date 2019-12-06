actor a {

  public func ping() : async () {
    debugPrint("ping! " # debug_show rts_callback_table_count());
  };

  public func go() = ignore async {
    debugPrint("go 1: " # debug_show rts_callback_table_count());
    let a1 = ping();
    debugPrint("go 1: " # debug_show rts_callback_table_count());
    let a2 = ping();
    debugPrint("go 1: " # debug_show rts_callback_table_count());
    let a3 = ping();
    debugPrint("go 1: " # debug_show rts_callback_table_count());
    let a4 = ping();

    debugPrint("go 1: " # debug_show rts_callback_table_count());
    await a1;
    debugPrint("go 2: " # debug_show rts_callback_table_count());
    await a2;
    debugPrint("go 3: " # debug_show rts_callback_table_count());
    await a3;
    debugPrint("go 4: " # debug_show rts_callback_table_count());
    await a4;
    debugPrint("go 5: " # debug_show rts_callback_table_count());
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
