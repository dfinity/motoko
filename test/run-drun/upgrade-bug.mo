import P "mo:⛔";

actor {
  stable var a : [var Nat] = [var];

  system func preupgrade() {
    a := P.Array_init<Nat>(268435456 / 4, 0x0F); // 0.25 GB array (I think)
    P.debugPrint("pre");
  };

  system func postupgrade() {
    P.debugPrint("post");
    P.trap("deliberate trap");
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
// too resource heavy on GH:
////SKIP comp

//CALL upgrade ""
