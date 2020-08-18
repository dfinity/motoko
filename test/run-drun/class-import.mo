import Prim "mo:prim";
import C "class:class-import/empty";
//import One "class:class-import/one";
import Two "class:class-import/two";

actor a {
  public func go() : async () {
    let p = await C();
    let blob = Prim.blobOfPrincipal (Prim.principalOfActor p);
    let c = blob.bytes();
    Prim.debugPrint(debug_show (c.next()));
    Prim.debugPrint(debug_show (c.next()));
    Prim.debugPrint(debug_show (c.next()));
    Prim.debugPrint(debug_show (c.next()));

    // test single arg class
//    let one = One("one");

    // test two arg class
    let two = Two("one","two");

  }
};

//SKIP run

a.go() //OR-CALL ingress go "DIDL\x00\x00"
