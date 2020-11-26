import Prim = "mo:prim";
import Cycles = "cycles/cycles";
import Lib = "actor-class-cycles/C";

actor a {

  public func go() : async () {
    Prim.debugPrint(debug_show({ Principal = Prim.principalOfActor a }));
    Prim.debugPrint(debug_show({ balance = Cycles.balance()}));
    if (Cycles.balance() == (0 : Nat64))
      await Cycles.provisional_top_up_actor(a, 10_000_000_000_000);
    Prim.debugPrint(debug_show({ balance = Cycles.balance()}));
    for (i in [/* 0, */ 1,2,3].vals()) {
      Prim.debugPrint(debug_show({ iteration = i}));
      Prim.debugPrint(debug_show({ balance = Cycles.balance()}));
      let c = await {
        Cycles.add(Prim.natToNat64(i*1_000_000_000_000));
	Lib.C();
      };
      Prim.debugPrint("ping");
      let b = await c.ping();
//      Prim.debugPrint(debug_show(await c.balance()));
    }
  }
};

a.go(); //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
