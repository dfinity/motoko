//MOC-FLAG --stable-regions
import P "mo:⛔";
import {new; size } "stable-region/Region";

// test region allocation is infinite
actor {

  public func go() : async() {
    var l = 65536*8;
    var n = 16; // first 16 regions are reserved
    loop {
      let r = new();
      assert P.regionId(r) == n;
//      P.debugPrint(debug_show {n; id = P.regionId(r)});
      assert size(r) == 0;
      n += 1;
    } while (n < l);
    P.debugPrint(debug_show {alloced = n});
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL ingress go "DIDL\x00\x00"

