import Array "array.mo";
import Debug "debug.mo";
import None "none.mo";

Debug.printLn("None");

{
  Debug.printLn("  absurd");

  func showNone(x : None) : Text {
    None.absurd<Text>(x);
  };
};
