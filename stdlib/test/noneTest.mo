import Array "mo:stdlib/array.mo";
import None "mo:stdlib/none.mo";
import Prelude "mo:stdlib/prelude.mo";

Prelude.printLn("None");

{
  Prelude.printLn("  absurd");

  func showNone(x : None) : Text {
    None.absurd<Text>(x);
  };
};
