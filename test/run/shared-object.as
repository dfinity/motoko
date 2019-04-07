type Shob = shared { a : Int; b : { c : ?Nat } };

let foo : Shob = shared { a = 17; b = shared { c = ?25 } };

// check whether we can pattern match shared objects

shared func baz (sh : Shob) : async Int = async (switch sh {
  case { a; b = { c = null } } a;
  case { a; b = { c = ?c } } (a + c)
});

shared func check (a : Int, b : Int) : async () = async {
  assert (a == b);
};


async {
  let b = await (baz foo);
  await (check (b, 42))
};
