type A = {};
type B = { a : Int };
type C = { a : Int; b : Text };

let a : A = {};
let b : B = { a = 0 };
let c : C = { a = 0; b = "" };

module Contravariant {
  module MA {
    public func cmp(_ : A, _ : A) : Nat = 1;
  };

  module MB1 {
    public func cmp(_ : B, _ : B) : Nat = 21;
  };

  module MB2 {
    public func cmp(_ : B, _ : B) : Nat = 22;
  };

  module MC {
    public func cmp(_ : C, _ : C) : Nat = 3;
  };

  func cmp<T>(x : T, y : T, implicit cmp : (T, T) -> Nat) : Nat {
    cmp(x, y);
  };

  public func diamond() {
    assert cmp(b, c) == 2;
  };
};

module Covariant {
  module MA {
    public let default : A = {};
  };

  module MB1 {
    public let default : B = { a = 0 };
  };

  module MB2 {
    public let default : B = { a = 17 };
  };

  module MC {
    public let default : C = { a = 0; b = "" };
  };

  func default<T>(implicit default : T) : T = default;

  public func diamond() {
    ignore default<B>();
  };
};

Contravariant.diamond();
Covariant.diamond();
