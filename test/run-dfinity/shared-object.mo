type Shob = { a : Int; b : { c : ?Nat } };

let foo : Shob = { a = 17; b = { c = ?25 } };

// check whether we can pattern match shared objects

shared func baz(sh : Shob) : future Int = future (switch sh {
  case {a; b = {c = null}} a;
  case {a; b = {c = ?c}} (a + c)
});

future {
  let b = await (baz foo);
  assert (b == 42);
};
