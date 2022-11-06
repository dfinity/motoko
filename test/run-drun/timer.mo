//MOC-ENV MOC_UNLOCK_PRIM=yesplease
//MOC-FLAG --experimental-field-aliasing
import {debugPrint; error; time} = "mo:⛔";

actor {
    // timer module implementation
    type Node = { var expire : Nat64; id : TimerId; delay : Nat64; recurring : Bool; job : () -> async (); var ante : ?Node; var dopo : ?Node };
    var timers : ?Node = null;
    var lastId = 0;
    
    // ad-hoc place for the Timer.mo API
    type TimerId = Nat;
    func addTimer(delay : Nat64, recurring : Bool, job : () -> async ()) : TimerId {
        lastId += 1;
        let id = lastId;
        let now = time();
        let expire = now + 1_000_000_000 * delay;
        func insert(n : ?Node, put : Node -> ()) =
            switch n {
            case null { put { var expire; id; delay; recurring; job; var ante = null; var dopo = null } };
            case (?n) {
                     if (n.expire == 0) { put { var expire; id; delay; recurring; job; var ante = n.ante; var dopo = n.dopo } }
                     else if (n.expire <= expire) { insert(n.dopo, func m = n.dopo := ?m) }
                     else { insert(n.ante, func m = n.ante := ?m) }
                 }
            };
        insert(timers, func n = timers := ?n);
        
        id
    };
    func cancelTimer(id : TimerId) {

    };


  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  var count = 0;
  var max = 5;

  system func timer() : async () {
    let now = time();


    type CNode = ?{var expire : Nat64; id : TimerId; delay : Nat64; recurring : Bool; ante : CNode; dopo : CNode };
    func clean(n : ?Node) : CNode = switch n {
                                        case null null;
                                        case (?n) ?{n with ante = clean(n.ante); dopo = clean(n.dopo) }
                                    };
    debugPrint(debug_show {now; timers = clean timers});
    
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

     let id = addTimer(1, false, func () : async () { debugPrint "YEP!" });
     let id2 = addTimer(2, false, func () : async () { debugPrint "DIM!" });

     
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
