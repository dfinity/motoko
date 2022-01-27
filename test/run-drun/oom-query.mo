// test incremental oom by allocating 5 GB, one GB at a time!
import P "mo:⛔";
actor {

  public query func go() : async () {
    var c = 5;

    while(c > 0) {
      let a : [var Nat8] = P.Array_init<Nat8>(1024*1024*1024/4, 0xFF);
      c -= 1;
    };
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
// too resource heavy on GH:
//CALL query go "DIDL\x00\x00"


