import E "env.mo";
import T "table.mo";
import P "prelude.mo";
import Array "../stdlib/array.mo";

P.printLn("Table");

{
  P.printLn("  len and read functions");
  let env = E.Env();
  let t = T.Table<Nat, Bool>(env, 0, true);
  assert(t.len() == 1);
  assert(t.read0(0) == 0);
  assert(t.read1(0) == true);

  t.write1(0, false);
  assert(t.len() == 1);
  assert(t.read0(0) == 0);
  assert(t.read1(0) == false);

  t.append(10,true);
  assert(t.len() == 2);
  assert(t.read0(1) == 10);
  assert(t.read1(1) == true);
};

{
  P.printLn("  eval");
  let env = E.Env();
  let t = T.Table<Nat, Bool>(env, 0, true);

  func f(x : Nat, b : Bool) : [Nat] { 
    if b [x] else [];
  };

  func eq(a : [Nat], b : [Nat]) : Bool {
    Array.equals<Nat>(a, b, func(x,y){x==y});
  };

  t.append(10,true);
  assert(eq(t.eval(f), [0,10]));

  t.write1(0,false);
  assert(eq(t.eval(f), [10]));

  t.write1(1,false);
  assert(eq(t.eval(f), []));

  t.write1(0,true);
  assert(eq(t.eval(f), [0]));
};
