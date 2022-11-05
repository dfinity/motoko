//MOC-ENV MOC_UNLOCK_PRIM=yesplease
import {debugPrint; error; time} = "mo:⛔";

actor {
    // timer module implementation
    type Node = { var expire : Nat64; delay : Nat64; recurring : Bool; job : () -> async (); var ante : ?Node; var dopo : ?Node };
    var timers : ?Node = null;
    var lastId = 0;
    
    // ad-hoc place for the Timer.mo API
    type TimerId = Nat;
    func addTimer(delay : Nat64, recurring : Bool, job : () -> async ()) : TimerId {
        lastId += 1;
        let now = time();
        let expire = now + 1_000_000_000 * delay;
        switch timers {
          case null { timers := ?{ var expire; delay; recurring; job; var ante = null; var dopo = null } };
        };
        
        lastId
    };
    func cancelTimer(id : TimerId) {

    };


  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  var count = 0;
  var max = 5;

  system func timer() : async () {
    if (count < max) {
      count += 1;
      await @run_timers();
      if (count == max) {
        let prev = (prim "global_timer_set" : Nat64 -> Nat64) 0;
        assert prev != 0;
      }
    }
  };

  public shared func go() : async () {
     var attempts = 0;
     let now = time();
     let prev = (prim "global_timer_set" : Nat64 -> Nat64)(now + 100_000_000);
     assert prev == 0;
     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (attempts >= 50 and count == 0)
         throw error("he's dead Jim");
     };
     debugPrint(debug_show {count});
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir

//CALL ingress go "DIDL\x00\x00"
