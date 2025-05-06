import { type T } "M0216/lib";

let t : T = 10;

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

func f(x : tVariant) {
  let (#left{ type N }) = x else {
    return ()
  };

  let n : N = 10;
}
