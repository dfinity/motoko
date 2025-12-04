let implicit(value) z : Nat = 0;

func addOne(value : (implicit : Int)) : Int {
  value + 1
};

func two() {
  switch (+1) {
    case implicit value {
      assert addOne() == 2;
    };
  };
};

two();
assert addOne() == 1;

module DumbHash {
  public func implicit(hashFn) hashNat(_ : Nat) : Nat { 1 };
  public func implicit(hashFn) hashInt(_ : Int) : Nat { 2 };
  public func implicit(hashFn) hashText(_ : Text) : Nat { 3 };
  public func implicit hashFn(_ : Char) : Nat { 4 };
};

func hash<T>(self : T, hashFn : (implicit : T -> Nat)) : Nat {
  hashFn(self)
};

assert hash(42) == 1;
assert hash(-2) == 2;
assert hash("Hello") == 3;
assert hash('H') == 4;
