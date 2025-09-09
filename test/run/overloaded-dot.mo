import ArrayExt "overloaded-dot/ArrayExt";

let Vec2 = module {
  public type Self = { x : Nat; y : Nat };

  public func add(v1 : Self, v2 : Self) : Self {
    { x = v1.x + v2.x; y = v1.y + v2.y }
  };
};

let Vec3 = module {
  public type Self = { x : Nat; y : Nat; z : Nat };

  public func add(v1 : Self, v2 : Self) : Self {
    { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z }
  };
};

module VarArrayExt {
  public type Self<T> = [var T];

  public func first<T>(self : [var T]) : T = self[0];
  public func map<A, B>(_self : [var A], _f : A -> B): [var B] {
    [var]
  };
};

module MyArrayExt {
  public type Self<T> = [T];
  public func sum(_xs : [Int]) : Int { -42 };
};

module NatExt {
  public type Self = Nat;
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
};

Main.vec();
Main.immutableArray();
Main.mutableArray();
Main.nat();
Main.overlappingSelf();
