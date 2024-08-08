//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import Prim "mo:⛔";

actor {

  func deserUnit(x : Blob) : () = (prim "deserialize" : Blob -> ()) x;

  public func go () : async () {
    try {
      await async { deserUnit "DIDL\01\6d\70\01\00\f0\f0\f0\f0\08"; };
      assert false
    }
    catch e {
      Prim.debugPrint(Prim.errorMessage(e));
    }
  }
}
//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref
//CALL ingress go "DIDL\x00\x00"
