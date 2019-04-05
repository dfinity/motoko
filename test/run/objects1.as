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
  case {a; b} b;
  case {a} 42
}
