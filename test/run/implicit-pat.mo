let z with value : Nat = 0;

func addOne(value : (implicit : Int)) : Int {
  value + 1
};

func two() {
  switch (+1) {
    case x with value {
      assert addOne() == 2;
    };
  };
};

two();
assert addOne() == 1;
