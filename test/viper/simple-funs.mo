// @verify

actor SimpleFuns {

  // return an Int constant
  public func getZero() : async Int {
    return 0;
  };

  // return a Bool constant
  public func getFalse() : async Bool {
    return false;
  };

  // take an Int argument and return it unmodified
  public func idInt(n : Int) : async Int {
    return n;
  };

  // take a Bool argument and return it unmodified
  public func idBool(b : Bool) : async Bool {
    return b;
  };

  // conditional early return from a function
  public func abs(n : Int) : async Int {
    if (n < 0) {
      return (0 - n);
    };
    return n;
  };

  // take multiple Int arguments, return Int
  public func max(n : Int, m : Int) : async Int {
    if (n >= m) {
      return n;
    };
    return m;
  };

  // take multiple Int arguments, return Bool
  public func eq4(n : Int, m : Int, p : Int, q : Int) : async Bool {
    return (n == m and m == p and p == q);
  };

  // return the value of a local mutable variable
  public func factorial(n : Int) : async Int {
    var prod : Int = 1;
    var i : Int = n;
    while (i > 0) {
      prod *= i;
      i -= 1;
    };
    return prod;
  };

  // take an Int and a Bool, return an Int
  public func bmul(b : Bool, n : Int) : async Int {
    if (b) {
      return n;
    };
    return 0;
  };

};
