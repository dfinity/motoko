//MOC-FLAG --stable-regions
import P "mo:⛔";
import {new; size} "stable-region/Region";

actor {

  public func go() : async() {
    var n = 16; // first 16 regions are reserved
    while (n < 32767) {
      var r1 = new();
      assert size(r1) == 0;
      n += 1;
    };
    P.debugPrint(debug_show {alloced = n});
    try {
      await async {
        let r = new();
        P.debugPrint("new failed to fail");
        assert false;
      }
    }
    catch e {
      P.debugPrint("ok");
    }
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL ingress go "DIDL\x00\x00"

