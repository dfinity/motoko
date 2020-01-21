import Prelude "mo:stdlib/prelude.mo";
import Text "mo:stdlib/text.mo";

Prelude.printLn("Text");

{
  Prelude.printLn("  append");

  let actual = Text.append("x", "y");
  let expected = "xy";

  assert(actual == expected);
};
