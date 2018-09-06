let a : var Nat[] = [1,2,42];

assert (a[0] == 1);
assert (a[1] == 2);
assert (a[2] == 42);

let b : var Nat[] = [2,3,23];

assert (b[0] == 2);
assert (b[1] == 3);
assert (b[2] == 23);

assert (a[0] == 1);
assert (a[1] == 2);
assert (a[2] == 42);

a[1] := 5;
b[1] := 6;
assert (a[1] == 5);
assert (b[1] == 6);
