//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
  persistent func initialPrint() {
    Prim.debugPrint("Initial function");
  };

  persistent func initialMap(x : Nat) : Text {
    "initial " # debug_show (x);
  };

  var print : persistent () -> () = initialPrint;
  var map : persistent Nat -> Text = initialMap;

  persistent func newPrint() {
    Prim.debugPrint("New function");
  };

  persistent func newMap(x : Nat) : Text {
    "new " # debug_show (x);
  };

  public persistent func change() : async () {
    print := newPrint;
    map := newMap;
  };

  print();
  Prim.debugPrint("Result: " # map(123));
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref
//CALL upgrade ""
//CALL ingress change "DIDL\x00\x00"
//CALL upgrade ""
