let a : var Nat[] = [0,1,0,0,0,0,0,0,0,0,0];

var i : Nat = 1;
while (i < 10) {
  i += 1;
  a[i] := a[i-1] + a[i-2];
};

assert (a[i] == 55);

