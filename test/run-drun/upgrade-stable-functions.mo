import Prim "mo:prim";

actor {
  func initialPrint() {
    Prim.debugPrint("Initial function");
  };

  func initialMap(x : Nat) : Text {
    "initial " # debug_show (x);
  };

  stable var print : () -> () = initialPrint;
  stable var map : Nat -> Text = initialMap;

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

//CALL upgrade ""
//CALL ingress change "DIDL\x00\x00"
//CALL upgrade ""
