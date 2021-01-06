import Prim "mo:prim";
func id<T>(x:T):T { x };

func foo(n : Nat8, b: Bool, t: Text ) {
//  if (n > (0 : Nat8)) { foo(n - (1:Nat8), not b, t # t ) };
  let x = id(666);
  let b1 = id(true);
  let t2 = id("hello");
  let blob = id("\FF\FFhello" : Blob);
  let n2 = id(66 : Nat8);
  let i = id(-66 : Int8);
  let c = id("abcdefghijklmnop") # id("qrstuvwxyz");
  let o = id({fa = 666; fb = "hello"; var fc = "state"});
  let z = id(null);
  let sz = id(? z);
  let ssz = id(? ? z);
  let sn = id(? 666);
  let ssn = id(? ? 666);

  let v0 = id(#fa);
  let v1 = id(#fb "data");
  let tup = id((x, b1, t2, blob, n2, i, c, o, z, sz, ssz, sn, ssn));

  Prim.debugPrint(debug_show(x, b1, t2, blob, n2, i, tup));
};

foo(6,true,"a");