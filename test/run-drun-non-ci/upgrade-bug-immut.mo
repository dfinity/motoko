import P "mo:⛔";

actor {
  stable var a : [Nat] = [];

  system func preupgrade() {
     a := P.Array_tabulate<Nat>(268435456 / 4, func _ { 0x0F } ); // 0.25 GB array (I think)
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

//CALL upgrade ""
