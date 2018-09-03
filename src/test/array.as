let a : var Nat[] = [1,2,42];

assert (a[0] == 1);
assert (a[1] == 2);
assert (a[2] == 42);

a[1] := 5;
assert (a[1] == 5);
