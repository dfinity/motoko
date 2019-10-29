module {
  import Int "int.mo";
  import Prelude "prelude.mo";

  public func add(x : Nat, y : Nat) : Nat {
    x + y;
  };

  public func toText(x : Nat) : Text {
    Int.toText(x);
  };
}
