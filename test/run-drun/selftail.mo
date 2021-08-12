import Prim "mo:⛔";
// a single function that can be evaluated recursively or tail-recursively
func f (tailCall:Bool, n:Int, acc:Int) : Int {
    if (n<=0)
	return acc;

    if (tailCall)
	f(tailCall, n-1, acc+1)
    else
	1 + f(tailCall, n-1, acc);
};

// check we get same results for small n
assert (f(false, 100, 0) == f(true, 100, 0));
Prim.debugPrint "ok1";

// check tail recursion works for large n
assert(100000 == f (true, 100000, 0));
Prim.debugPrint "ok2";

// check recursion overflows for large n (overflows on drun only)
// this would throw a stack overflow in drun; but we do not run
// this in our test suite, as the output differs from linux to darwin etc.
// assert(100000 == f (false, 100000, 0));
// Prim.debugPrint "ok3 (unreachable on drun)";
