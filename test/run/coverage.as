func f() {
  let _ = 0;
  let a = 0;
  let 5 = 0;
  let (5 or 5) = 0;
  let (5 or _) = 0;
  let (_ or 6) = 0;
  let (_ or _) = 0;
  let ?b = ?0;
  let ?_ = ?0;
  let ?9 = ?0;

  func f1(_ : Nat) {};
  func f2(x : Nat) {};
  func f3(5) {};
  func f4(5 or 5) {};
  func f5(5 or _ : Nat) {};
  func f6(_ or 6 : Nat) {};
  func f7((_ or _) : Nat) {};

  switch 0 { case _ {} };
  switch 0 { case x {} };
  switch 0 { case 5 {} };
  switch 0 { case 5 {}; case 5 {} };
  switch 0 { case 5 {}; case _ {} };
  switch 0 { case 5 {}; case x {} };
  switch 0 { case _ {}; case 6 {} };
  switch 0 { case _ {}; case x {} };
  switch 0 { case x {}; case _ {} };
  switch 0 { case x {}; case x {} };
  switch 0 { case _ {}; case _ {} };
  switch 0 { case (5 or 6) {}; case (7 or 6) {} };
  switch (0, 0) { case (_, _) {}; case (_, 6) {} };
  switch (0, 0) { case (_, (6 or _)) {}; case _ {} };
  switch (0, 0) { case (0, _) {}; case (_, 0) {} };
  switch (0, 0) { case (0, _) {}; case (_, 0) {}; case _ {} };
};


type Tree = {#leaf : Int; #branch : (Tree, Tree)};

// leaf is not fully covered and branch is covered twice
func size(t : Tree) : Nat {
  switch t {
  case (#leaf 3) 1;
  case (#branch(t1, t2)) { 1 + size(t1) + size(t2) };
  case (#branch(t1, t2)) { 1 + size(t1) + size(t2) };
  }
};

// the following is fully covered
func size1(t : Tree) : Nat {
  switch t {
  case (#leaf 3) 1;
  case (#branch(t1, t2)) { 1 + size1(t1) + size1(t2) };
  case (#leaf _) 1;
  }
};

// example from Sestoft's paper
// ML pattern match compilation and partial evaluation
type Lam =
 { #va : Int
 ; #lam : (Int, Lam)
 ; #app : (Lam, Lam)
 ; #le : (Int, Lam, Lam)
 };

func test (t : Lam) : Nat = switch (t) {
  case (#va x) 111;
  case (#lam(x, #va y)) 222;
  case (#lam(x, #lam(y, z))) 333;
  case (#lam(x, #app(y, z))) 444;
  case (#app(#lam(x, y), z)) 555;
  case (#app(#app(x, y), z)) 666;
  case (#le(x, #le(y, z, v), w)) 777;
  case (#lam(x, #le(y, z, v))) 888;
  case (#le(x, y, #app(z, v))) 999;
  case (#app(#app(#lam(x, #lam(y, z)), v), w)) 1010;

  case (#app(_, _)) 2200;
  case (#le(_, _, _)) 2300;
};

assert (test (#le(1, #va 0, #app(#va 0, #va 1))) == 999)
