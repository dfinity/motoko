import Prim "mo:prim";

actor {
  stable var length = 0;
  stable var stableArray : [Nat] = [];

  Prim.debugPrint("Upgrade instructions: " # debug_show (Prim.rts_upgrade_instructions()));

  public func increase() : async () {
    if (length == 0) {
      length := 1_000;
    } else {
      length *= 10;
    };
    let newArray = Prim.Array_tabulate<Nat>(
      length,
      func(index) {
        index * index;
      },
    );
    stableArray := newArray;
  };
};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL ingress increase "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress increase "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress increase "DIDL\x00\x00"
//CALL upgrade ""
