import Prim "mo:prim";
// This is like local-throw.as, just not local
// (Using `await async { … }` around relevant parts

actor a {
  public func t2() : async () {
     try {
       await async {
         throw error("t2");
         assert(false);
      }
     } catch e {
          switch (errorCode(e),errorMessage(e)) {
            case (#error, "t2") { };
            case (#system, _ ) { assert false;};
            case (#error, _) { assert false;};
       }
     }
  };

  public func t3() : async () {
    try {
      await async {
        try {
          await async {
            throw error("t3");
            assert(false);
          }
        } catch e1 {
          switch (errorCode(e1), errorMessage(e1)) {
            case (#error, "t3") {
              throw error("t31");
            };
            case (#system, _) {
              assert false;
            };
            case (#error, _) {
              assert false;
            };
          }
        }
      }
    }
    catch e2 {
      switch (errorCode(e2),errorMessage(e2)) {
       case (#error, "t31") { };
       case (#system, _) {
         assert false;
       };
       case (#error, _) {
         assert true;
       };
      }
    }
  };


  public func go() = ignore async {
    try {
      await t2();
      Prim.debugPrint ("t2 ok");
    } catch _ {
      assert false;
    };

    try {
      await t3();
      Prim.debugPrint ("t3 ok");
    } catch _ {
      assert false;
    };
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
