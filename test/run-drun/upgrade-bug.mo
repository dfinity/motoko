import P "mo:⛔";

actor {
  stable var a : [var Nat] = P.Array_init(268435456 / 4, 0x0F); // 0.25 GB array (I think)

  system func preupgrade() { P.debugPrint("pre"); };

  system func postupgrade() { P.debugPrint("post"); }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
// too resource heavy on GH:
//SKIP comp

//CALL upgrade ""
