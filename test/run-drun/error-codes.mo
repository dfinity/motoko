import P = "mo:prim";

// TODO: test #sys_fatal, #sys_transient and #future somehow
// (I don't know how to trigger the first two; and the third needs a future error code)

actor a {

  flexible let invalid = actor "IC:C0FEFED00D41" : actor { request() : async () };

  public func trap() : async () { assert false; };

  public func throwText(t : Text) : async () { throw P.error(t); };

  public func go() : async () {

    try {
        await invalid.request();
    }
    catch e {
      switch (P.errorCode(e), P.errorMessage(e)) {
        case (#destination_invalid, m) { P.debugPrint("destination_invalid:" # m)};
        case _ { assert false; }
      }
    };

    try {
        await throwText("ball");
    }
    catch e {
      switch (P.errorCode(e), P.errorMessage(e)) {
        case (#canister_reject, m) {
          assert (m == "ball");
          P.debugPrint("canister_reject:"#m);};
        case _ { assert false; }
      }
    };

    try {
        await trap();
    }
    catch e {
      switch (P.errorCode(e), P.errorMessage(e)) {
        case (#canister_error, m) { P.debugPrint("canister_error:" # m)};
        case _ { assert false; }
      }
    };

    try ()
      // nuthin
    catch e {
      switch (P.errorCode(e)) {
        case (#sys_fatal) { assert false};
        case (#sys_transient) { assert false};
        case (#destination_invalid) {assert false};
        case (#canister_error) { assert false};
        case (#canister_reject) { assert false};
        case (#future (w : Word32)) {assert false};
      };
    }


  }
};

ignore(a.go()); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKUP run-ir
//SKIP run-low
