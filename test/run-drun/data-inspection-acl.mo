import Prim "mo:⛔";
import Lib "data-inspection-acl/C";

// Test `__motoko_inspect_data` can only be invoked from self or controller.
actor Self {

  type DataInspection = actor {
    __motoko_inspect_data: () -> async Blob;
  };

  func withDataInspection(a : actor {}) : DataInspection {
    actor (debug_show (Prim.principalOfActor(a))) : DataInspection;
  };

  public func go() : async () {
    let self = withDataInspection(Self);

    // Test call from self.
    do {
      try {
        ignore await self.__motoko_inspect_data();
        Prim.debugPrint("Self.__motoko_inspect_data()");
      }
      catch _e {
        assert false;
      }
    };

    let controlee = await (Lib.C(self));

    // Test call from controller.
    do {
      try {
        ignore await withDataInspection(controlee).__motoko_inspect_data();
        Prim.debugPrint("controlee.__motoko_inspect_data()");
      }
      catch _e {
        assert false;
      }
    };

    // Test callback from non-controller.
    do {
      try {
        ignore await controlee.callback();
        assert false;
      }
      catch e {
        Prim.debugPrint(Prim.errorMessage(e));
      }
    }
  }
}

//CALL ingress go "DIDL\x00\x00"
