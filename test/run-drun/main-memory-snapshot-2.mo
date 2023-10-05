import Prim "mo:⛔";
actor {

  type PageList = ?{ head : Blob; tail : PageList };
  type Snapshots = ?{ region : Region; tail : Snapshots };
 
  stable var pages : PageList = null;
  stable var snapshots : Snapshots = null;
     
  public func grow(numPages : Nat) {
      let pageSize = 1 << 16 : Nat32;
      let bytes = Prim.Array_init<Nat8>(numPages * (Prim.nat32ToNat(pageSize)), 0 : Nat8);
      pages := ?{ head = Prim.arrayMutToBlob(bytes); tail = pages };
  };

  // returns snapshot size, in pages.
  public func createSnapshot() : Nat {
      let r = Prim.regionNew();
      snapshots := ?{ region = r ; tail = snapshots };
      Prim.regionMainMemorySnapshot(r);
      Prim.regionSize(r)
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref

//CALL ingress grow "DIDL\x00\x01\x7d\x01"
//CALL ingress grow "DIDL\x00\x01\x7d\x80\x0a"
//CALL ingress createSnapshot "DIDL\x00\x00"

