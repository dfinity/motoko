type Shob = { a : Int; b : { c : ?Nat } };

let foo : Shob = new { a = 17; b = new { c = ?25 } };

// check whether we can pattern match shared objects

shared func baz(sh : Shob) : async Int = async (switch sh {
  case {a; b = {c = null}} a;
  case {a; b = {c = ?c}} (a + c)
});

async {
  let b = await (baz foo);
  assert (b == 42);
};
