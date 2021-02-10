import Prim "mo:prim";
import Lib = "clone";

actor cloneMaker {

   public shared func makeClone(init : Nat): async Lib.Clone {
      await Lib.Clone(makeClone,init);
   };

   public shared func test() : async () {

      // create the original Clone object
      let c0 : Lib.Clone = await makeClone(0);
      await c0.someMethod();
      Prim.debugPrint(debug_show(Prim.principalOfActor c0));

      // create some proper clones
      let c1 : Lib.Clone = await c0.clone(1); // clone!
      await c1.someMethod();
      Prim.debugPrint(debug_show(Prim.principalOfActor c1));

      let c2 : Lib.Clone = await c1.clone(2); // clone!
      await c2.someMethod();
      Prim.debugPrint(debug_show(Prim.principalOfActor c2));
   }

};

// testing
//SKIP run
//SKIP run-ir
//SKIP run-low
ignore cloneMaker.test(); //OR-CALL ingress test "DIDL\x00\x00"
