import Prelude "prelude.mo";
import Text "text.mo";

Debug.printLn("Text");

{
  Debug.printLn("  append");

  let actual = Text.append("x", "y");
  let expected = "xy";

  assert(actual == expected);
};
