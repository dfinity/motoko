import Prim "mo:prim";
// This file only exercises local throws that don't cross function boundaries.
// In principle, it should run on all targets.

actor a {
  public func t2() : async () {
     try {
       throw Prim.error("t2");
       assert(false);
     } catch e {
          switch (Prim.errorCode(e),Prim.errorMessage(e)) {
            case (#error, "t2") { };
            case (#system, _ ) { assert false;};
            case (#error, _) { assert false;};
       }
     }
  };

  public func t3() : async () {
    try {
      try {
        throw Prim.error("t3");
        assert(false);
      } catch e1 {
        switch (Prim.errorCode(e1), Prim.errorMessage(e1)) {
          case (#error, "t3") {
            throw Prim.error("t31");
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
    catch e2 {
      switch (Prim.errorCode(e2),Prim.errorMessage(e2)) {
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
