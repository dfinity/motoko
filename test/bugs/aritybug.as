
// fails in the interpreter at line * with  ../../src/asc -r  u.as
// fails in the interpreter at line * with  ../../src/asc -r -a u.as
// but succeeds  ../../src/asc -r -a -A u.as

// the problem is that the type-checker accepts line * because  (Int,) and Int domain types
// aren't distinguished after arity raising, since Type.as_seq Int = Type.as_seq (Tup[Int]),
// eventhough Int != Tup[Int]

{
let t = "u_u\n";
shared func fu_u(a:Int,) : async (Int,) {
   return (2*a,);
};

let _ : async (Int,)  = async {
  let (x,) = await fu_u(1); // *
  assert(x==2);
  print t;
  return (x,);
};
};
