import Prim "mo:prim";
import C "class:class-import/empty";

actor a {
  public func go() {
    let p = C();
    let blob = Prim.blobOfPrincipal (Prim.principalOfActor p);
    let c = blob.bytes();
    Prim.debugPrint(debug_show (c.next()));
    Prim.debugPrint(debug_show (c.next()));
    Prim.debugPrint(debug_show (c.next()));
    Prim.debugPrint(debug_show (c.next()));
  }
};

//SKIP run

a.go() //OR-CALL ingress go "DIDL\x00\x00"
