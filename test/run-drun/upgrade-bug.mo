import P "mo:⛔";

actor {
  stable var a : [var Nat] = P.Array_init(268435456,0x0F); // 1GB array

  system func preupgrade() { P.debugPrint("pre"); };

  system func postupgrade() { P.debugPrint("post"); }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL upgrade ""
