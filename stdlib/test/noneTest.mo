import Array "mo:stdlib/Array";
import None "mo:stdlib/None";
import Prelude "mo:stdlib/Prelude";

Prelude.printLn("None");

{
  Prelude.printLn("  absurd");

  func showNone(x : None) : Text {
    None.absurd<Text>(x);
  };
};
