//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import Prim "mo:⛔";

actor {
  func inline(t : Text) : () = (prim "printText" : Text -> ()) t;
  func outline(t : Text) : () = (prim "printText" : Text -> ()) "sometext";
  public func go() : async () {
    inline("inline me"); // should be inlined
    outline("outline me"); // should not be inlined, but a direct call
  }
}

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP comp-ref
//CALL ingress go "DIDL\x00\x00"
