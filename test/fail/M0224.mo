module A {
  public type Self = { x : Nat };
  public func getX(self : Self) : Nat { self.x }
};

module B {
  public type Self = { x : Int };
  public func getX(self : Self) : Int { self.x };
  public func getX1(self : Self) : Int { self.x + 1 };
};

module C {
  public type Self = { x : Int };
  public func getX1(self : Self) : Int { self.x + 1 };
};

let a : A.Self = { x = 10 };
let b : B.Self = { x = +10 };

ignore a.getX(); // Fine (with disambiguation)
ignore b.getX(); // Fine
ignore b.getX1(); // overlaps
