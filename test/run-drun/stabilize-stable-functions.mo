
//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
//MOC-FLAG --stabilization-instruction-limit=10000
import Prim "mo:prim";

actor {
  func initialPrint() {
    Prim.debugPrint("Initial function");
  };

  func initialMap(x : Nat) : Text {
    "initial " # debug_show (x);
  };

  stable var print : stable () -> () = initialPrint;
  stable var map : stable Nat -> Text = initialMap;

  func newPrint() {
    Prim.debugPrint("New function");
  };

  func newMap(x : Nat) : Text {
    "new " # debug_show (x);
  };

  public func change() : async () {
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

//CALL ingress __motoko_stabilize_before_upgrade "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress __motoko_destabilize_after_upgrade "DIDL\x00\x00"
//CALL ingress change "DIDL\x00\x00"
