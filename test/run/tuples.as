let tup1 = (1,2);
let (x,y) = tup1;
assert(x == 1);
assert(y == 2);

let nested1 = (1,(2,3));
let (_,nested2) = nested1;
let (_,z) = nested2;
assert (z == 3);

let (_,(_,a)) = nested1;
assert (a == 3);

type T = (Nat, T);
func f(x : T) { let (n, _) = x; };
