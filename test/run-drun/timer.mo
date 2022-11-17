//MOC-ENV MOC_UNLOCK_PRIM=yesplease
//MOC-FLAG --experimental-field-aliasing
import {Array_init; debugPrint; error; time} = "mo:⛔";

actor {
    // timer module implementation
    type Node = { var expire : Nat64; id : TimerId; delay : ?Nat64; job : () -> async (); var ante : ?Node; var dopo : ?Node };
    var timers : ?Node = null;
    var lastId = 0;


    func nextExpiration(n : ?Node) : Nat64 {
        switch n {
        case null 0;
        case (?n) {
                 var exp = nextExpiration(n.ante);
                 if (exp == 0) {
                     exp := n.expire;
                     if (exp == 0) {
                         exp := nextExpiration(n.dopo)
                     }
                 };
                 exp
             }
        }
    };


    // ad-hoc place for the Timer.mo API
    type TimerId = Nat;
    func addTimer(delay : Nat64, recurring : Bool, job : () -> async ()) : TimerId {
        lastId += 1;
        let id = lastId;
        let now = time();
        let expire = now + 1_000_000_000 * delay;
        func insert(n : ?Node, put : Node -> ()) =
            switch n {
            case null { put { var expire; id; delay = if recurring ?delay else null; job; var ante = null; var dopo = null } };
            case (?n) {
                     if (n.expire == 0) { put { var expire; id; delay = if recurring ?delay else null; job; var ante = n.ante; var dopo = n.dopo } }
                     else if (n.expire <= expire) { insert(n.dopo, func m = n.dopo := ?m) }
                     else { insert(n.ante, func m = n.ante := ?m) }
                 }
            };
        insert(timers, func n = timers := ?n);


        let exp = nextExpiration timers;
        let prev = (prim "global_timer_set" : Nat64 -> Nat64) exp;
        /*FIXME: this is expensive*/
        if (prev != 0 and prev != 0 and exp > prev) {
            // reinstall
            ignore (prim "global_timer_set" : Nat64 -> Nat64) prev;
        };

        debug { debugPrint(debug_show {now; exp; prev; id }) };

        id
    };

    func cancelTimer(id : TimerId) {
        func hunt(n : ?Node) = switch n {
          case null ();
          case (?n) { if (n.id == id) {
                          n.expire := 0
                      };
                      hunt(n.ante);
                      hunt(n.dopo)
               }
        };
        hunt timers
    };


  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  var count = 0;
  var max = 5;

  system func timer() : async () {
    let now = time();

    type CNode = ?{var expire : Nat64; id : TimerId; delay : ?Nat64; ante : CNode; dopo : CNode };
    func clean(n : ?Node) : CNode =
      switch n {
          case null null;
          case (?n) ?{n with ante = clean(n.ante); dopo = clean(n.dopo) }
      };
    //debugPrint(debug_show {now; timers = clean timers});

    let exp = nextExpiration timers;
    let prev = (prim "global_timer_set" : Nat64 -> Nat64) exp;

    if (exp == 0) {
        return
    };

    var gathered = 0;
    let thunks : [var ?(() -> async ())] = Array_init(10/*FIXME*/, null);
    //let next = now + 1_000_000_000;
    
    func gatherExpired(n : ?Node) : () {
        switch n {
        case null ();
        case (?n) {
                 gatherExpired(n.ante);
                 if (n.expire > 0 and n.expire <= now) {
                     thunks[gathered] := ?(n.job);
                     n.expire := 0;
                     gathered += 1;
                 };
                 gatherExpired(n.dopo);
             }
        }
    };

    gatherExpired(timers);

    let futures : [var ?(async ())] = Array_init(thunks.size(), null);
    for (k in thunks.keys()) {
        futures[k] := switch (thunks[k]) { case (?thunk) ?(thunk()); case _ null };
    };

    debug { debugPrint(debug_show { gathered; exp }) };

    for (f in futures.vals()) {
        switch f { case (?f) { await f }; case _ () }
    };
  };

  public shared func go() : async () {
     var attempts = 0;
     let now = time();

     let id1 = addTimer(1, false, func () : async () { count += 1; debugPrint "YEP!" });
     let id2 = addTimer(2, false, func () : async () { count := max; debugPrint "DIM!" });
     //let id3 = addTimer(3, false, func () : async () { count := max; debugPrint "ROOK!" });

     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       //if (count > 1) { cancelTimer id };
       if (attempts >= 100 and count == 0)
         throw error("he's dead Jim");
     };
     debugPrint(debug_show {count});
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run

//CALL ingress go "DIDL\x00\x00"
