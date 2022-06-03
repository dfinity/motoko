import Prim "mo:⛔";

actor {

  public func go() : async () {
     let c1 = Prim.performanceCounter(0);
     var n = 1000_0000_000;
     while (n > 0) {
        n-=1;
     };
     let c2 = Prim.performanceCounter(0);
     assert (c2 > c1);
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:

//CALL go "DIDL\x00\x00"
