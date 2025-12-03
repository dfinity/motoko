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
  func hashNat_(_ : Nat) : Nat { 1 };
  func hashInt_(_ : Int) : Nat { 2 };
  func hashText_(_ : Text) : Nat { 3 };

  public let implicit(hashFn) hashNat : Nat -> Nat = hashNat_;
  public let implicit(hashFn) hashInt : Int -> Nat = hashInt_;
  public let implicit(hashFn) hashText : Text -> Nat = hashText_;
};

func hash<T>(self : T, hashFn : (implicit : T -> Nat)) : Nat {
  hashFn(self)
};

assert hash(42) == 1;
assert hash(-2) == 2;
assert hash("Hello") == 3;
