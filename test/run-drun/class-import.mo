import Prim "mo:prim";
import C "class:class-import/empty";
import One "class:class-import/one";
import Two "class:class-import/two";

actor a {
 public func go() : async () {
    let empty = await C();
    await empty.test();

    // test single arg class
    let one = await One("one");
    await one.test();

    // test two arg class
    let two = await Two("one","two");
    await two.test();
  }
};

//SKIP run

a.go() //OR-CALL ingress go "DIDL\x00\x00"
