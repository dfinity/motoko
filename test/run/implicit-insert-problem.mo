func foo(a : Nat, b : (implicit : Nat), c : Nat, d : (implicit : Nat)) : (Nat, Nat, Nat, Nat) = (a, b, c, d);

let b = 2;
let d = 4;
func testAllProvided() {
  assert foo(1, b, 3, d) == (1, 2, 3, 4);
};
func testNoneProvided() {
  assert foo(1, 3) == (1, 2, 3, 4);
};
func testFirstProvided() {
  assert foo(1, b, 3) == (1, 2, 3, 4);
};
func testSecondProvided() {
  assert foo(1, 3, d) == (1, 2, 3, 4);
};
testAllProvided();
testNoneProvided();
testFirstProvided();
testSecondProvided();
