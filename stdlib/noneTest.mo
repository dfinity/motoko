import Array "array.mo";
import None "none.mo";
import Prelude "prelude.mo";

Prelude.printLn("None");

{
  Prelude.printLn("  absurd");

  func showNone(x : None) : Text {
    None.absurd<Text>(x);
  };
};
