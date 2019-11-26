actor a {

  public func ping() : async () {
    debugPrint("ping! " # debug_show rts_outstanding_callbacks());
  };

  public func go() = ignore async {
    debugPrint("go 1: " # debug_show rts_outstanding_callbacks());
    let a1 = ping();
    debugPrint("go 1: " # debug_show rts_outstanding_callbacks());
    let a2 = ping();
    debugPrint("go 1: " # debug_show rts_outstanding_callbacks());
    let a3 = ping();
    debugPrint("go 1: " # debug_show rts_outstanding_callbacks());
    let a4 = ping();

    debugPrint("go 1: " # debug_show rts_outstanding_callbacks());
    await a1;
    debugPrint("go 2: " # debug_show rts_outstanding_callbacks());
    await a2;
    debugPrint("go 3: " # debug_show rts_outstanding_callbacks());
    await a3;
    debugPrint("go 4: " # debug_show rts_outstanding_callbacks());
    await a4;
    debugPrint("go 5: " # debug_show rts_outstanding_callbacks());
  };

  go();
}

//SKIP run
//SKIP run-low
//SKIP run-ir
