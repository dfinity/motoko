//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import {debugPrint; error; time} = "mo:⛔";

actor {

  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  var count = 0;
  var max = 10;

  system func timer() : async () {
      assert false;
    if (count < max) {
      count += 1;
      await @run_timers();
    }
  };

  public shared func go() : async () {
     var attempts = 0;
     let now = time();
     let prev = (prim "global_timer_set" : Nat64 -> Nat64)(now + 2 * 1_000_000_000);
     assert prev == 0;
     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (attempts >= 500 and count == 0)
         throw error("he's dead Jim");
     };
     debugPrint(debug_show {count});
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir

//CALL ingress go "DIDL\x00\x00"
