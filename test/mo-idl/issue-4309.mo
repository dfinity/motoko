//MOC-FLAG --actor-idl issue-4309

import Types = "./import/issue-4309-types";
import Types2 = "./import/issue-4309-types";

actor {
  module Inner {
    public type Foo = Types.Foo;
    public type Credit<T> = Nat;
    public type Binary<T, U> = (T, U);
    public type Ternary<T, U, V> = (T, U, V);
    public type BinaryInv<T, U> = Binary<U, T>;
    public type TernaryInv<T, U, V> = Ternary<V, U, T>;
  };

  module Innerer {
      public type Credit<T> = Inner.Credit<T>;
      public type Binary<T,U> = Inner.Binary<T,U>;
      public type BinaryInv<T, U> = Inner.BinaryInv<T, U>;
      public type Ternary<T, U, V> = Inner.Ternary<T, U, V >;
      public type TernaryInv<T,U, V> = Inner.TernaryInv<T, U, V >;
  };

  public type Credit = Innerer.Credit<Nat>;
  public type Foo = Inner.Foo;

  public type Binary = Innerer.Binary<Nat, Int>;
  public type Ternary = Innerer.Ternary<Nat, Int, Bool>;

  public func get() : async (Credit, Inner.Credit<Nat>, Innerer.Credit<Nat>) {
      (0, 0, 0)
  };
  public func silly(_ : Foo, _ : Inner.Foo, _ : Types.Foo, _ : Types2.Foo) : async () { };

  public func binary() : async (Binary, Inner.Binary<Nat, Int>, Innerer.Binary<Nat, Int>) {
      loop {};
  };

  public func ternary() : async (Ternary, Inner.Ternary<Nat, Int, Bool>, Innerer.Ternary<Nat, Int, Bool>) {
      loop {};
  };

  public func binaryInv() : async (Binary, Inner.BinaryInv<Nat, Int>, Innerer.BinaryInv<Nat, Int>) {
      loop {};
  };

  public func ternaryInv() : async (Ternary, Inner.TernaryInv<Nat, Int, Bool>, Innerer.TernaryInv<Nat, Int, Bool>) {
      loop {};
  };

}
