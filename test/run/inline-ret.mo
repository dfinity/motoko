func id(x : Bool) : Bool { return x };

var x = true;
func foo() {
  x := false;
  assert(id(true));
  x := true; // this must not be skipped
};
foo();
assert(x);
