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
