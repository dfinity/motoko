//MOC-FLAG --actor-idl issue-4309

import Types = "./issue-4309-types";
import Types2 = "./issue-4309-types";

actor {
  module Inner {
    public type Foo = Types.Foo;
    public type Credit<T> = Nat;
  };

  module Innerer {
      public type Credit<T> = Inner.Credit<T>;
  };

  public type Credit = Innerer.Credit<Nat>;
  public type Foo = Inner.Foo;
  public func get() : async (Credit, Inner.Credit<Nat>, Innerer.Credit<Nat>) {
      (0, 0, 0)
  };
  public func silly(_ : Foo, _ : Inner.Foo, _ : Types.Foo, _ : Types2.Foo) : async () { }
}
