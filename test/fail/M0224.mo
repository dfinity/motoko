module A {
  public type Self = { x : Nat };
  public func getX(self : Self) : Nat { self.x }
};

module B {
  public type Self = { x : Int };
  public func getX(self : Self) : Int { self.x }
};

let a : A.Self = { x = 10 };
let b : B.Self = { x = +10 };

ignore b.getX(); // Fine
ignore a.getX(); // overlapping resolution
