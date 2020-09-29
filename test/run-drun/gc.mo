import Prim "mo:prim";
import Random "gc/Random";

actor {
    public shared func test() : async () {
      let block = await Random.blob();
      let fin = Random.Finite block;
      func CRASHER () : async () {
        let f : Random.Finite = fin; // uncomment, and the crash appears (because `fin` is closed over, thus retained in GC?)
      };
      await CRASHER();
      await CRASHER();
    };

    public shared func go() : async () {
      var i = 3;
      while(i > 0) {
        Prim.debugPrint(debug_show(i));
        await test();
        i -= 1;
      }
    };
}

//CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
