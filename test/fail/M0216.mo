import { type T; type Result<Ok, Err> } "M0216/lib";

let _t : T = 10;
let _r1 : Result<Nat, Text> = #ok(3);
let _r2 : Result<Nat, Text> = #err("Woot");

module What {
  public type S<A> = A;
};

type Left = module {
  type N = Nat;
};

type Right = module {
  type N = Text;
};

type tVariant = { #left : Left; #right : Right };

func _f(x : tVariant) {
  let (#left{ type N }) = x else {
    return ()
  };

  let _n : N = 10;
};

func _f2(x : tVariant) {
  let (#left{ type N } or #right { type N }) = x else {
    return ()
  };
  let _n : N = 10;
};
