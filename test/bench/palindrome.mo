// The 2 palindrome implementations from
// "There and Back Again", by Olivier Danvy and Mayer Goldberg
//
import { performanceCounter; rts_heap_size; debugPrint } = "mo:⛔";

actor Palindrome {
    type List<A> = ?(A, List<A>);

    func cps(xs : List<Char>) : Bool {
        func walk(xs1 : List<Char>, xs2 : List<Char>, k : List<Char> -> Bool) : Bool =
            switch (xs1, xs2) {
                 case (_, null) { k xs1 };
                 case (?(_, xs1), ?(_, null)) { k xs1 };
                 case (?(x, xs1), ?(_, ?(_, xs2))) { walk (xs1, xs2, func (?(y, ys)) = x == y and k ys) }
             };
        walk (xs, xs, func null = true)
    };

    func direct(xs : List<Char>) : Bool {
        func walk (xs1, xs2) = switch (xs1, xs2) {
            case (_, null) xs1; // even length
            case (_ :: xs1, _ :: nil) xs1 // odd length
            case (x :: xs1, _ :: _ :: xs2) {
                if x == y (
                    let ?(y, ys) = walk (xs1, xs2);
                    ys
                } else { throw Error.reject("Nope") };
            }
        }
        try {
            let null = walk (xs0, xs0);
            true
        } catch _ false
    }

    func as_list(cs : Text) : List<Char> {
        var l : List<Char> = null;
        for (c in cs.chars()) {
            l := ?(c, l)
        };
        l
    };

    func counters() : (Int, Nat64) = (rts_heap_size(), performanceCounter(0));

    public func go() : async () {
        let (m0, n0) = counters();
        let b = cps(as_list "go hang a salami imalas a gnah og"); // Go hang a salami, I'm a lasagna hog.
        let (m1, n1) = counters();
        debugPrint(debug_show (b, m1 - m0, n1 - n0));

        let (i0, j0) = counters();
        let c = direct(as_list "go hang a salami imalas a gnah og");
        let (i1, j1) = counters();
        debugPrint(debug_show (c, i1 - i0, j1 - j0))
    }
}

//CALL ingress go 0x4449444C0000
