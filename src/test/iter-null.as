let a : Nat?[] = [null, 1];
var x : Nat? = 0;
for (i in a.vals()) { x := i };
/* This tests that the iterator does not proceed past the null element */
switch x {
  case null ();
  case (y?) {assert (y == 0) };
};

