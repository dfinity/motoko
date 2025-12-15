import ArrayExt "contextual-dot/ArrayExt";

let Vec2 = module {
  public type Vec2 = { x : Nat; y : Nat };

  public func add(self : Vec2, v2 : Vec2) : Vec2 {
    { x = self.x + v2.x; y = self.y + v2.y }
  };
};

let Vec3 = module {
  public type Vec3 = { x : Nat; y : Nat; z : Nat };

  public func add(self : Vec3, v2 : Vec3) : Vec3 {
    { x = self.x + v2.x; y = self.y + v2.y; z = self.z + v2.z }
  };
};

module VarArrayExt {
  public func first<T>(self : [var T]) : T = self[0];
  public func map<A, B>(self : [var A], _f : A -> B): [var B] {
    let _ = self;
    [var]
  };
  public func append<T>(self : [var T], _elem : T) : [var T] { self };
};

module MyArrayExt {
  public func sum(self : [Int]) : Int {
    let _ = self;
    -42
  };
};

module NatExt {
  public func twice(self : Nat) : Nat { self * 2 };
};

module Main {
  public func vec() {
    let x = { x = 10; y = 20 };
    let x1 = x.add({ x = 30; y = 40 });
    assert x1.x == 40;
  };

  public func immutableArray() {
    let arr : [Nat] = [10];
    assert arr.first() == 10;

    let arr2 = arr.map(func _ = "Hello");
    assert arr2.size() == 0;
  };

  public func mutableArray() {
    let varArr : [var Nat] = [var 10];
    let varArr2 = varArr.map(func _ = "Hello");
    assert varArr2.size() == 0
  };

  public func nat() {
    let ten = (5).twice();
    assert ten == 10;
  };

  public func overlappingSelf() {
    let xs : [Int] = [+10];
    // Although the imported ArrayExt defines a matching Self its exported `sum`
    // function does not have a matching type, so this unambigously resolves
    // to MyArrayExt.sum in this file
    // Trying to call `.sum()` on a `[Nat]` would however cause an error however
    // as `[Nat] <: [Int]`
    assert xs.sum() == -42;
  };

  public func tupleIssue() {
    let xs : [var (Nat, Text)] = [var];
    assert xs.append((3, "hello")).size() == 0
  };

  public func localFunc() {
    func thrice(self : Nat) : Nat {
      self * 3;
    };
    func twice(self : Nat) : Nat {
      self * 3;
    };
    assert (5).thrice() == 15;
    // Local definition takes precedence
    assert (5).twice() == 15;
    do {
      func twice(self : Nat) : Nat {
        self * 4;
      };
      // Shadowing works as expected
      assert (5).twice() == 20;
    }
  }
};

Main.vec();
Main.immutableArray();
Main.mutableArray();
Main.nat();
Main.overlappingSelf();
Main.localFunc();
