import Prim "mo:⛔";

// test compilation of local async functions with non-trivial patterns
actor a {

  // trivial argument pattern
  func f0(r : { trap : Bool }) : async () {
    Prim.debugPrint ("f0");
  };

  // irrefutable argument pattern
  func f1({ trap : Bool }) : async () {
    Prim.debugPrint ("f1");
  };

  // refutable argument
  func f2({ trap = false : Bool }) : async () {
    Prim.debugPrint ("f2");
  };

  public func go() : async () {

    await f0({ trap = false });

    await f1({ trap = false });

    await f2({ trap = false });

    try await async {
       ignore f2({ trap = true }); // should trap, not produce async
       Prim.debugPrint ("f2() failed to trap");
       assert false;
    }
    catch e {
       Prim.debugPrint ("f2() trapped successfully");
    };
  }
};

await a.go()//OR-CALL ingress go "DIDL\x00\x00"
