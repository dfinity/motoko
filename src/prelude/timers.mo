// default timer mechanism implementation
// fundamental node invariant: max_exp ante <= expire <= min_exp dopo
// corollary: if expire == 0 then the ante is completely expired
//
type @TimerId = Nat;
type @Node = { var expire : Nat64; id : @TimerId; delay : ?Nat64; job : () -> async (); ante : ?@Node; dopo : ?@Node };
var @timers : ?@Node = null;
func @prune(n : ?@Node) : ?@Node = switch n {
    case null null;
    case (?n) {
        if (n.expire == 0) {
            @prune(n.dopo) // by corollary
        } else {
            ?{ n with ante = @prune(n.ante); dopo = @prune(n.dopo) }
        }
    }
};

func @nextExpiration(n : ?@Node) : Nat64 = switch n {
    case null 0;
    case (?n) {
        var exp = @nextExpiration(n.ante); // TODO: use the corollary for expire == 0
        if (exp == 0) {
            exp := n.expire;
            if (exp == 0) {
                exp := @nextExpiration(n.dopo)
            }
        };
        exp
    }
};

// Function called by backend to run eligible timed actions.
// DO NOT RENAME without modifying compilation.
func @run_timers() : async () {
    func Array_init<T>(len : Nat,  x : T) : [var T] {
        (prim "Array.init" : <T>(Nat, T) -> [var T])<T>(len, x)
    };
    let now = (prim "time" : () -> Nat64)();
    let exp = @nextExpiration @timers;
    let prev = (prim "global_timer_set" : Nat64 -> Nat64) exp;

    if (exp == 0) {
        return
    };

    var gathered = 0;
    let thunks : [var ?(() -> async ())] = Array_init(10, null); // we want max 10

    func gatherExpired(n : ?@Node) = switch n {
        case null ();
        case (?n) {
            gatherExpired(n.ante);
            if (n.expire > 0 and n.expire <= now and gathered < thunks.size()) {
                thunks[gathered] := ?(n.job);
                switch (n.delay) {
                    case (?delay) {
                        // re-add the node
                        let expire = n.expire + delay;
                        // N.B. insert only works on pruned nodes
                        func reinsert(m : ?@Node) : @Node = switch m {
                            case null ({ n with var expire; ante = null; dopo = null });
                            case (?m) {
                                assert m.expire != 0;
                                if (expire < m.expire) ({ m with ante = ?reinsert(m.ante) })
                                else ({ m with dopo = ?reinsert(m.dopo) })
                            }
                        };
                        @timers := ?reinsert(@prune(@timers));
                    };
                    case _ ()
                };
                n.expire := 0;
                gathered += 1;
            };
            gatherExpired(n.dopo);
        }
    };

    gatherExpired(@timers);

    let futures : [var ?(async ())] = Array_init(thunks.size(), null);
    for (k in thunks.keys()) {
        futures[k] := switch (thunks[k]) { case (?thunk) ?thunk(); case _ null };
    };

    for (f in futures.vals()) {
        switch f { case (?f) { await f }; case _ () }
    };
};

var @lastId = 0;

func @setTimer(delaySecs : Nat64, recurring : Bool, job : () -> async ()) : @TimerId {
    @lastId += 1;
    let id = @lastId;
    let now = (prim "time" : () -> Nat64) ();
    let delayNanos = 1_000_000_000 * delaySecs;
    let expire = now + delayNanos;
    let delay = if recurring ?delayNanos else null;
    // only works on pruned nodes
    func insert(n : ?@Node) : @Node =
        switch n {
            case null ({ var expire; id; delay; job; ante = null; dopo = null });
            case (?n) {
                assert n.expire != 0;
                if (expire < n.expire) ({ n with ante = ?insert(n.ante) })
                else ({ n with dopo = ?insert(n.dopo) })
            }
    };
    @timers := ?insert(@prune(@timers));

    let exp = @nextExpiration @timers;
    if (exp == 0) @timers := null;
    let prev = (prim "global_timer_set" : Nat64 -> Nat64) exp;
    /*FIXME: this is expensive*/
    if (prev != 0 and prev != 0 and exp > prev) {
        // reinstall
        ignore (prim "global_timer_set" : Nat64 -> Nat64) prev;
    };

    id
};

func @cancelTimer(id : @TimerId) {
    func graft(onto : ?@Node, branch : ?@Node) : ?@Node = switch (onto, branch) {
        case (null, null) null;
        case (null, _) branch;
        case (_, null) onto;
        case (?onto, _) { ?{ onto with dopo = graft(onto.dopo, branch) } }
    };
    func hunt(n : ?@Node) : ?@Node = switch n {
        case null n;
        case (?{ id = node; ante; dopo }) {
            if (node == id) {
                graft(ante, dopo)
            } else do? {
                { n! with ante = hunt ante; dopo = hunt dopo }
            }
        }
    };

    @timers := hunt @timers;

    if (@nextExpiration @timers == 0) {
        // no more expirations ahead
        ignore (prim "global_timer_set" : Nat64 -> Nat64) 0;
        @timers := null
    }
};
