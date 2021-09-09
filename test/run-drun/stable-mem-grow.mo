import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";

actor {

  stable var n : Nat32 = 0;

  system func preupgrade() {
    P.debugPrint("upgrading from " # debug_show n);
    let m = StableMemory.grow(1);

    // check all pages clear
    var i : Nat32 = 0;
    let max = StableMemory.size() * 65536;
    while (i < max) {
      assert (StableMemory.loadNat32(i) == 0);
      i += 4
    };

    n += 1;

  };

  public func testGrow() : async () {
    var i : Nat32 = 0;
    while (i < 10) {
      var pre = StableMemory.size();
      var post = StableMemory.grow(i);
      assert StableMemory.size() == StableMemory.grow(0);
      assert post == pre;
      assert StableMemory.size() == pre + i;
      i += 1;
    }
  };

  system func postupgrade() {
    P.debugPrint("to " # debug_show n);
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL upgrade ""
//CALL ingress testGrow "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress testGrow "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress testGrow "DIDL\x00\x00"
//CALL upgrade ""

