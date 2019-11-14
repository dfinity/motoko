import Array "array.mo";
import None "none.mo";
import Prelude "prelude.mo";

Debug.printLn("None");

{
  Debug.printLn("  absurd");

  func showNone(x : None) : Text {
    None.absurd<Text>(x);
  };
};
