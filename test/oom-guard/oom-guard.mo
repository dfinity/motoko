import P "mo:⛔";
// exercise OOM detection
// allocate up to last page, and then incrementally try to
// trigger OOM incrementally on last page.
// post-upgrade traps to avoid hitting disk
// NB: the oom would not be detected pre 0.6.21
actor {

  ignore P.stableMemoryGrow(1);

  func nearlyOom() {
    while (P.rts_memory_size() / 65536 < 65535) {
     ignore P.stableMemoryLoadBlob(0, 37268);
    };
  };

  public query func testQuery() : async () {
    P.debugPrint "testQuery";
    nearlyOom();
    P.trap "testQuery success";
  };

  public func testUpdate() : async () {
    P.debugPrint "testUpdate";
    nearlyOom();
    P.trap "testUpdate success";
  };

  system func preupgrade() {
    P.debugPrint "preupgrade";
    nearlyOom();
  };

  system func postupgrade() {
    P.debugPrint("upgraded");
    P.trap "postupgrade success";
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref

//CALL ingress testQuery "DIDL\x00\x00"
//CALL ingress testUpdate "DIDL\x00\x00"
//CALL upgrade ""

