func nonrec(a : {x : Nat}) = ignore {a = {c = 2}; b = a.c};

nonrec({x = 0});
nonrec({x = 1; y = 0;});
nonrec({x = 1; y = 0; var z = 2});

let o : {a : {}; b : Nat} = {a = {x = 0}; b = a.x};
let o2 : {var a : Nat} = {var a = 0};
let o3 : {} = {};

func foo() : Int = switch (o) {
  case {b = 11} 22;
  case {b = 1 : Int; a = {}} 0;
  case {b = b : Int; a} 1 + b;
  case {b : Nat} 42;
  case {a} 43;
};

assert (foo() == 1);
