// @verify

actor LoopInvariant {
  public func double(n : Int) : async Int {
    var i : Int = 0;
    var j : Int = 0;
    while (i < n) {
      assert:loop:invariant (i * 2 == j);
      assert:loop:invariant (j - i == i);
      i := i + 1;
      j := j + 2;
    };
    return j;
  };
}