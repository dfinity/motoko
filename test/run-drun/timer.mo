//MOC-ENV MOC_UNLOCK_PRIM=yesplease
//MOC-FLAG --experimental-field-aliasing
import {Array_init; debugPrint; error; time} = "mo:⛔";

actor {
    // timer module implementation
    // node invariant max_exp ante <= expire <= min_exp dopo
    // corollary: if expire == 0 then the ante is completely expired
    type Node = { var expire : Nat64; id : TimerId; delay : ?Nat64; job : () -> async (); ante : ?Node; dopo : ?Node };
    var timers : ?Node = null;
    var lastId = 0;


    func nextExpiration(n : ?Node) : Nat64 = switch n {
        case null 0;
        case (?n) {
            var exp = nextExpiration(n.ante); // TODO: use the corollary for expire == 0
            if (exp == 0) {
                exp := n.expire;
                if (exp == 0) {
                    exp := nextExpiration(n.dopo)
                }
            };
            exp
        }
    };

    func prune(n : ?Node) : ?Node = switch n {
       case null null;
       case (?n) {
                if (n.expire == 0)
                { prune(n.dopo) } // by corollary
                else
                { ?{ n with ante = prune(n.ante); dopo = prune(n.dopo) } }
            }
    };

    // ad-hoc place for the Timer.mo API
    type TimerId = Nat;
    func setTimer(delaySecs : Nat64, recurring : Bool, job : () -> async ()) : TimerId {
        lastId += 1;
        let id = lastId;
        let now = time();
        let delayNanos = 1_000_000_000 * delaySecs;
        let expire = now + delayNanos;
        let delay = if recurring ?delayNanos else null;
        // only works on pruned nodes
        func insert(n : ?Node) : Node =
            switch n {
              case null ({ var expire; id; delay; job; ante = null; dopo = null });
              case (?n) {
                 assert n.expire != 0;
                 if (expire < n.expire) ({ n with ante = ?(insert(n.ante)) })
                 else ({ n with dopo = ?(insert(n.dopo)) })
               }
            };
        timers := ?insert(prune(timers));

        let exp = nextExpiration timers;
        if (exp == 0) timers := null;
        let prev = (prim "global_timer_set" : Nat64 -> Nat64) exp;
        /*FIXME: this is expensive*/
        if (prev != 0 and prev != 0 and exp > prev) {
            // reinstall
            ignore (prim "global_timer_set" : Nat64 -> Nat64) prev;
        };

        //debug { debugPrint(debug_show {now; exp; prev; id }) };

        id
    };

    func graft(onto : ?Node, branch : ?Node) : ?Node = switch (onto, branch) {
        case (null, null) null;
        case (null, _) branch;
        case (_, null) onto;
        case (?onto, _) { ?{ onto with dopo = graft(onto.dopo, branch) } }
    };

    func cancelTimer(id : TimerId) {
        func hunt(n : ?Node) : ?Node = switch n {
          case null n;
          case (?{ id = node; ante; dopo }) {
                   if (node == id) {
                       graft(ante, dopo)
                   } else do? {
                       { n! with ante = hunt ante; dopo = hunt dopo }
                   }
               }
        };

        timers := hunt timers;

        if (nextExpiration timers == 0) {
            // no more expirations ahead
            ignore (prim "global_timer_set" : Nat64 -> Nat64) 0;
            timers := null
        }
    };


  let raw_rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  var count = 0;
  var max = 10;

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
    
    func gatherExpired(n : ?Node) =
        switch n {
        case null ();
        case (?n) {
                 gatherExpired(n.ante);
                 if (n.expire > 0 and n.expire <= now) {
                     thunks[gathered] := ?(n.job);
                     switch (n.delay) {
                       case (?delay) {
                         // re-add the node
                         let expire = n.expire + delay;
                         // N.B. insert only works on pruned nodes
                         func insert(m : ?Node) : Node =
                           switch m {
                             case null ({ n with var expire; ante = null; dopo = null });
                             case (?m) {
                                      assert m.expire != 0;
                                      if (expire < m.expire) ({ m with ante = ?(insert(m.ante)) })
                                      else ({ m with dopo = ?(insert(m.dopo)) })
                                  }
                         };
                         timers := ?insert(prune(timers));
                       };
                       case _ ()
                     };
                     n.expire := 0;
                     gathered += 1;
                 };
                 gatherExpired(n.dopo);
             }
        };

    gatherExpired(timers);

    let futures : [var ?(async ())] = Array_init(thunks.size(), null);
    for (k in thunks.keys()) {
        futures[k] := switch (thunks[k]) { case (?thunk) ?(thunk()); case _ null };
    };

    //debug { debugPrint(debug_show { gathered; exp }) };

    for (f in futures.vals()) {
        switch f { case (?f) { await f }; case _ () }
    };
  };

  public shared func go() : async () {
     var attempts = 0;

     let id1 = setTimer(1, false, func () : async () { count += 1; debugPrint "YEP!" });
     let id2 = setTimer(2, true, func () : async () { count += 1; debugPrint "DIM!" });
     let id3 = setTimer(3, false, func () : async () {
         count += 1;
         debugPrint "ROOK!";
         ignore setTimer(1, true, func () : async () { count += 1; debugPrint "BATT!" })
     });

     while (count < max) {
       ignore await raw_rand(); // yield to scheduler
       attempts += 1;
       if (count > 5) { cancelTimer id2 };
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
