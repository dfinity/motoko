import Prim "mo:prim";
import C "class-import/empty";
import One "class-import/one";
import Two "class-import/two";
import Trap "class-import/trap";

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

    // test non-trapping install
    try {
      let trap = await Trap(false);
    }
    catch _ {
      assert false;
    };

    // test trapping install
    try {
      let trap = await Trap(true);
      assert false;
    }
    catch _ {
      Prim.debugPrint("caught trap");
    };

  }
};

a.go() //OR-CALL ingress go "DIDL\x00\x00"
