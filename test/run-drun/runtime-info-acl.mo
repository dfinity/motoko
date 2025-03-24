import Prim "mo:⛔";
import Lib "runtime-info-acl/C";
import Info "runtime-info/info"
// Test `__motoko_runtime_information` can only be invoked from self or controller.
actor Self {
  public func go() : async () {
    // Test call from Self
    do {
      try {
        ignore await Info.introspect(Self).__motoko_runtime_information();
        Prim.debugPrint("Self information okay");
      }
      catch _e {
        assert false;
      }
    };

    let c = await (Lib.C(Info.introspect(Self)));

    // Test call from controller (Self is a controller of c).
    do {
      try {
        ignore await Info.introspect(c).__motoko_runtime_information();
        Prim.debugPrint("Controllee information okay");
      }
      catch _e {
        assert false;
      }
    };

    // Test callback from non-controller (c is not a controller of Self).
    do {
      try {
        await c.callback();
        assert false;
      }
      catch e {
        Prim.debugPrint(Prim.errorMessage(e));
      }
    }
  }
}

//CALL ingress go "DIDL\x00\x00"
