module A {
  public type T = { x : Nat };
  public func getX(self : T) : Nat { self.x }
};

module B {
  public type T = { x : Int };
  public func getX(self : T) : Int { self.x };
  public func getX1(self : T) : Int { self.x + 1 };
};

module C {
  public type T = { x : Int };
  public func getX1(self : T) : Int { self.x };
};

let a : A.T = { x = 10 };
let b : B.T = { x = +10 };

ignore a.getX(); // Fine (with disambiguation)
ignore b.getX(); // Fine
ignore b.getX1(); // overlaps
