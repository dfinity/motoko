let p = new {x = 3; private y = 2; get_y() : Int = y};
assert(p.x == 3);
assert(p.get_y() == 2);

let o : {a : {}; b : Nat} = new {a = new {x = 0}; b = a.x};

let oo = object {
  private type T = Int;
  let x : T = 3;
  let (y, z) = (3, "");
  var v = 0;
  func f() : T { g() + x };
  func g() : T { f() + y };
  private class C() {};
};

// pattern matching

func get_b () : Int = switch (o) {
  case {b = 11} 22;
  case {b = result} result;
  case {b = b : Int; a} b;
  case {a} 42
};

assert(get_b () == 0);

// subtyping in patterns

let q : {a : Int; b : Nat} = new {a = -42; b = 25};

func get_a () : Int = switch (q) {
  case {a = 42; b} b;                  // OK: 42 is Int by subtyping
  case {b = 25 : Nat; a = a : Int} a   // OK: Int can consume all Nats
};

assert (get_a () == -42);

// subtyping and tuple patterns for comparison

let row : (Nat, Int, {c : Char; d : Text}) = (100, -42, new {c='C'; d="D"});

func foo () : Int = switch row {
  case (a : Int, -42, {c} : {c : Char}) (word32ToNat(charToWord32 c))  // OK
};

assert (foo () == 67)
