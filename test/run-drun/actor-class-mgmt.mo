import Prim "mo:⛔";
import Cycles = "cycles/cycles";
import C "actor-class-mgmt/C";

actor a {

  let ic00 = actor "aaaaa-aa" :
    actor {
      create_canister : {
        settings : ? {
          controllers : ?[Principal];
          compute_allocation: ?Nat;
          memory_allocation: ?Nat;
        freezing_threshold: ?Nat;
       }
     } -> async { canister_id : Principal };
   };

  let default_settings = { settings = null };
  // same as default but explicit
  let settings = { settings = ? {
     controllers = null;
     compute_allocation = null;
     freezing_threshold = null;
     memory_allocation = null;
    };
  };

  public func go () : async () {
    // To get lots of cycles in both drun and ic-ref-run
    if (Cycles.balance() == 0)
      await Cycles.provisional_top_up_actor(a, 100_000_000_000_000);

    do {
      Cycles.add(2_000_000_000_000);
      let c0 = await
         C.C 0;
      assert ({args = 0; upgrades = 0} == (await c0.observe()));

      Cycles.add(2_000_000_000_000);
      let c1 = await
         C.installC (#new default_settings) 1;
      assert ({args = 1; upgrades = 0} == (await c1.observe()));
      assert (c1 != c0);

      Cycles.add(2_000_000_000_000);
      let c2 = await
         (C.installC (#new settings) 2);
      assert ({args = 2; upgrades = 0} == (await c2.observe()));
      assert (c2 != c1);

      Cycles.add(2_000_000_000_000);
      let {canister_id = p} = await
         ic00.create_canister default_settings;
      // no need to add cycles
      let c3 = await
         C.installC (#install p) 3;
      assert ({args = 3; upgrades = 0} == (await c3.observe()));
      assert (Prim.principalOfActor c3 == p);
      assert (c3 != c2);

      // no need to add cycles
      let c4 = await
         C.installC (#upgrade c3) 4;
      assert ({args = 4; upgrades = 1} == (await c4.observe()));
      assert (c4 == c3);

      // no need to add cycles
      let c5 = await
         C.installC (#reinstall c4) 5;
      assert ({args = 5; upgrades = 0} == (await c5.observe()));
      assert (c5 == c4);
    };

  };

}

//CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
