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
  case {a; b = b : Int} b;
  case {a} 42
};

assert(get_b () == 0);

// subtyping in patterns

let q : {a : Int; b : Nat} = new {a = -42; b = 25};

func get_a () : Int = switch (q) {
  //case {a = 25 : Nat} 1;   // NOT OK: Nat cannot consume all Ints
  case {a = 42; b} b;        // OK: 42 is Int by subtyping
  case
  {a = a : Int;
   b = 25 : Int} a;          // OK: Int can consume all Nats
};

// the above is analogous to the simpler:

func bar (a : Int) = switch a {
  case 25 ();           // OK: 25 is Int by subtyping
  case (25 : Int) ();   // OK: 25 is Int by ascription
  //case (a : Nat) ()   // NOT OK: Nat cannot consume all Ints
};


// subtyping and tuple patterns for comparison (delete later)

let row : (Nat, Int, Nat) = (100, -42, 25);

func foo () : Int = switch row {
  //case (25, a : Nat, -25) 42;     // NOT OK: Nat cannot consume all Ints
  case (a : Int, -25, 25 : Int) 0   
}