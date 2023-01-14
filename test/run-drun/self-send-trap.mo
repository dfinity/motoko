//MOC-FLAG --trap-on-call-error
//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import Prim "mo:⛔";

actor self {

  func showError(e : Error) : Text = debug_show (Prim.errorCode(e), Prim.errorMessage(e));

  let MAX_SELF_QUEUE_CAPACITY = 500;
  let PRED_MAX_SELF_QUEUE_CAPACITY = MAX_SELF_QUEUE_CAPACITY - 1 : Nat;
  let DOUBLE_CAPACITY = 2 * MAX_SELF_QUEUE_CAPACITY;

  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  public func request() : async () {
  };

  public func oneway() : () {
  };

  public func test1() : async () {
    var n = 0;
    while (n < DOUBLE_CAPACITY) {
//      Prim.debugPrint(debug_show n);
      ignore request();
      n += 1;
    }

  };

  public func test2() : async () {
    try {
      var n = 0;
      while (n < DOUBLE_CAPACITY) {
//        Prim.debugPrint(debug_show n);
        ignore request();
        n += 1;
      }
    } catch e {
      assert false; // can't be caught
    }
  };


  public func test3() : async () {
    var n = 0;
    while (n < DOUBLE_CAPACITY) {
//      Prim.debugPrint(debug_show n);
      oneway(); // never trap
      n += 1;
    }

  };

  public func test4() : async () {
    try {
      var n = 0;
      while (n < DOUBLE_CAPACITY) {
//        Prim.debugPrint(debug_show n);
        oneway();
        n += 1;
      }
    } catch e {
      assert false;
      Prim.debugPrint("caught " # showError(e));
      throw e;
    }
  };

  public func test5() : async () {
    var n = 0;
    while (n < DOUBLE_CAPACITY) {
//    Prim.debugPrint(debug_show n);
      // NB: calling
      // ignore Prim.call_raw(Prim.principalOfActor(self),"request", to_candid ());
      // is not enough to trigger message send failure, because the Prim.call_raw is an
      // eta-expansion of prim "call_raw", and introduces an additional await, draining the queue.
      // Instead, we need to call the raw primitive:
      ignore (prim "call_raw" : (Principal, Text, Blob) -> async Blob) (Prim.principalOfActor(self),"request", to_candid ());
      //ignore request();
      n += 1;
    }

  };

  public func test6() : async () {
    try {
      var n = 0;
      while (n < DOUBLE_CAPACITY) {
//        Prim.debugPrint(debug_show n);
      // NB: calling
      // ignore Prim.call_raw(Prim.principalOfActor(self),"request", to_candid ());
      // is not enough to trigger message send failure, because the Prim.call_raw is an
      // eta-expansion of prim "call_raw", and introduces an additional await, draining the queue.
      // Instead, we need to call the raw primitive:
      ignore (prim "call_raw" : (Principal, Text, Blob) -> async Blob) (Prim.principalOfActor(self),"request", to_candid ());
        n += 1;
      }
    } catch e {
      Prim.debugPrint("caught " # showError(e));
      throw e;
    }
  };

  public func test7() : async () {
    var n = 0;
    var a = async ();
    await a;
    while (n < PRED_MAX_SELF_QUEUE_CAPACITY) {
//      Prim.debugPrint(debug_show n);
      ignore request();
      n += 1;
    };
    await a;
    assert false;
  };

  public func test8() : async () {
    try {
      var n = 0;
      var a = async ();
      await a;
      while (n < PRED_MAX_SELF_QUEUE_CAPACITY) {
        ignore request();
        n += 1;
      };
      await a;
    } catch e {
      Prim.debugPrint("caught " # showError(e));
      throw e;
    }
  };


  // local async functions
  func local() : async () {
  };

  public func test9() : async () {
    var n = 0;
    while (n < DOUBLE_CAPACITY) {
//      Prim.debugPrint(debug_show n);
      ignore local();
      n += 1;
    }

  };

  public func test10() : async () {
    try {
      var n = 0;
      while (n < DOUBLE_CAPACITY) {
//        Prim.debugPrint(debug_show n);
        ignore local();
        n += 1;
      }
    } catch e {
      Prim.debugPrint("caught " # showError(e));
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
      Prim.debugPrint("test1: " # showError(e));
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    Prim.debugPrint("test2:");
    try {
      await test2();
      assert false;
    }
    catch e {
      Prim.debugPrint("test2: " # showError(e));
    };

    Prim.debugPrint("test3:");

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    try {
      await test3();
    }
    catch e {
      Prim.debugPrint("test3: " # showError(e));
      assert false;
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    Prim.debugPrint("test4:");
    try {
      await test4();
    }
    catch e {
      assert false;
      Prim.debugPrint("test4: " # showError(e));
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    // call_raw
    Prim.debugPrint("test5:");

    try {
      await test5();
      assert false;
    }
    catch e {
      Prim.debugPrint("test5: " # showError(e));
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    Prim.debugPrint("test6:");
    try {
      await test6();
      assert false;
    }
    catch e {
      Prim.debugPrint("test6: " # showError(e));
    };


    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    // completed awaits
    Prim.debugPrint("test7:");

    try {
      await test7();
      assert false;
    }
    catch e {
      Prim.debugPrint("test7: " # showError(e));
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    Prim.debugPrint("test8:");
    try {
      await test8();
      assert false;
    }
    catch e {
      Prim.debugPrint("test8: " # showError(e));
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    // local functions

    Prim.debugPrint("test9:");

    try {
      await test9();
      assert false;
    }
    catch e {
      Prim.debugPrint("test9: " # showError(e));
    };

    let _ = await raw_rand(); // drain queues, can't use await async() as full!

    Prim.debugPrint("test10:");
    try {
      await test8();
      assert false;
    }
    catch e {
      Prim.debugPrint("test10: " # showError(e));
    };


  }

};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run

//await a.go(); //OR-CALL ingress go "DIDL\x00\x00"
