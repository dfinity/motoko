import Prim "mo:⛔";

actor a {

  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;


  public func test1() : async () {
    var n = 0;
    while (n < 1000) {
//      Prim.debugPrint(debug_show n);
      ignore async ();
      n += 1;
    }

  };

  public func test2() : async () {
    try {
      var n = 0;
      while (n < 1000) {
//        Prim.debugPrint(debug_show n);
        ignore async ();
        n += 1;
      }
    } catch e {
      Prim.debugPrint("caught " # Prim.errorMessage(e));
      throw e;
    }
  };

  public func go() : async () {

    Prim.debugPrint("test1:");

    try {
      await test1();
      assert false;
    }
    catch e {
      Prim.debugPrint("test1: " # Prim.errorMessage(e));
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    Prim.debugPrint("test2:");
    try {
      await test2();
    }
    catch e {
      Prim.debugPrint("test2: " # Prim.errorMessage(e));
    };

  }

};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run

//await a.go(); //OR-CALL ingress go "DIDL\x00\x00"
