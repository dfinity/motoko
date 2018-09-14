class C() {};
class G<T>() {};
actor class A() {};
actor class H<T>() {};

assert(C() is C);
assert(G<Int>() is G);
assert(A() is A);
assert(H<Int>() is H);

assert(not ({} is C));
assert(not ({} is G));
assert(not ({} is A));
assert(not ({} is H));
assert(not (actor {} is C));
assert(not (actor {} is G));
assert(not (actor {} is A));
assert(not (actor {} is H));

assert(not (C() is A));
assert(not (C() is G));
assert(not (G<Int>() is C));
assert(not (G<Int>() is H));
assert(not (A() is C));
assert(not (A() is H));
assert(not (H<Int>() is A));
assert(not (H<Int>() is G));

let c : Class = C;
let g : Class = G;
let a : Class = A;
let h : Class = H;

func f(c : class () -> {}) : {} = c();
let x = f(C);
func k(a : class () -> actor {}) : actor {} = a();
let y = k(A);
