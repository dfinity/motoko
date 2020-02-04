import Array "mo:stdlib/array";
import None "mo:stdlib/none";
import Prelude "mo:stdlib/prelude";

Prelude.printLn("None");

{
  Prelude.printLn("  absurd");

  func showNone(x : None) : Text {
    None.absurd<Text>(x);
  };
};
