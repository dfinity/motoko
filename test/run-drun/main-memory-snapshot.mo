import Prim "mo:⛔";
actor {
  stable var byte_pattern : Text = "Hello world";
  stable var snapshotRegion1 = Prim.regionNew();
  stable var snapshotRegion2 = Prim.regionNew();
  public func go() : async Text {
    Prim.regionMainMemorySnapshot(snapshotRegion1);
    Prim.debugPrint("hello world!");
    let p = byte_pattern;
    Prim.regionMainMemorySnapshot(snapshotRegion2);
    byte_pattern := "goodbye";
    Prim.debugPrint("goodbye!");
    p
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref
//CALL ingress go 0x4449444C0000

