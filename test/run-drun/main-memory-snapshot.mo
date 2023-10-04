import Prim "mo:⛔";
actor {
  stable var byte_pattern : Text = "Hello world";
  stable var snapshotRegion1 = Prim.regionNew();
  stable var snapshotRegion2 = Prim.regionNew();
  public func hello() : async Text {
    Prim.regionMainMemorySnapshot(snapshotRegion1);
    let p = byte_pattern;
    Prim.debugPrint("Hello World 1!");
    Prim.debugPrint("Hello World 2!");
    byte_pattern := "goodbye";
    Prim.regionMainMemorySnapshot(snapshotRegion2);
    p
  }
}

//CALL hello 0x4449444C0000 []
