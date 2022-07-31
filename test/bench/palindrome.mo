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

    //func direct(seed : Nat) : ([Int32], Nat) {};
    fun pal_d xs0
    = let fun
 = let
exception FALSE
fun walk (xs1, nil)
= xs1
| walk
= xs1
| walk
  (* even length *)
(_ :: xs1, _ :: nil)
  (*  odd length *)
(x :: xs1, _ :: _ :: xs2)
= let
in if x = y
val (y :: ys) = walk (xs1, xs2)
then ys
   else raise FALSE
end
in let val nil = walk (xs0, xs0) in true
end handle FALSE => false end

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
/*
        let (i0, j0) = counters();
        let (_, it) = arrInt32(13);
        let (i1, j1) = counters();
        debugPrint(debug_show (it, i1 - i0, j1 - j0))*/
    }
}

//CALL ingress go 0x4449444C0000
