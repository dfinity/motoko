import E "../stdlib/env.mo";
import C "../stdlib/cell.mo";
import A "../stdlib/array.mo";
import P "prelude.mo";

P.printLn("Cell");

{
  P.printLn("  all functions");
	let env = E.Env();
  let b = C.Cell<Bool>(env, true);

  assert(b.read() == true);
  b.write(false);
  assert(b.journal().len() == 2);
  assert(A.equals<Bool>(b.values(), [true,false], func(x,y){x==y}));

  let a = C.Cell<Nat>(env, 3);
  assert(a.read() == 3);
  a.write(2);
  assert(a.journal().len() == 2);
  assert(A.equals<Nat>(a.values(), [3,2], func(x,y){x==y}));
};
