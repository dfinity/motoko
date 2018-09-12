let a : Nat[] = [1, 2, 42];

assert(a.len() == 3);

assert (a[0] == 1);
assert (a[1] == 2);
assert (a[2] == 42);

assert (a.get(0) == 1);
assert (a.get(1) == 2);
assert (a.get(2) == 42);

let b : var Nat[] = [2, 3, 23];

assert(b.len() == 3);

assert (b[0] == 2);
assert (b[1] == 3);
assert (b[2] == 23);

assert (b.get(0) == 2);
assert (b.get(1) == 3);
assert (b.get(2) == 23);

b[1] := 6;
assert (b[1] == 6);

b.set(2, 7);
assert (b[2] == 7);
