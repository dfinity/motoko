import Int "int.as";
import Prelude "prelude.as";

module {
  public func add(x : Nat, y : Nat) : Nat {
    x + y;
  };

  public func toText(x : Nat) : Text {
    Int.toText(x);
  };
}
