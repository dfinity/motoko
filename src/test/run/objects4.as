let oddeven = new this {
  odd(n : Int)  : Bool = if (n > 0) this.odd(n - 1) else false;
  even(n : Int) : Bool = if (n > 0) this.even(n - 1) else true;
};
assert (oddeven.even(10));
