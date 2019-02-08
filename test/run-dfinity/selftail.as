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
print "ok1 \n";

// check tail recursion works for large n
assert(10000 == f (true, 10000, 0));
print "ok2 \n";

// check recursion overflows for large n (on dvm only)
// disabled as overflowing or not appears to be non-deterministic on V8
//assert(10000 == f (false, 10000, 0));
//print "unreachable on dvm \n";
