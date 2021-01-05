func foo(n : Nat8, b: Bool, t: Text ) {
  let x = 666;
  let b1 = true;
  let t2 = "hello";
  let n2:Nat8 = 66;
  let i:Int8 = 66;
  if (n > (0 : Nat8)) { foo(n - (1:Nat8), not b, t # t ) };
};

foo(6,true,"a");