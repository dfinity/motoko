import Prelude "mo:stdlib/Prelude";
import Text "mo:stdlib/Text";

Prelude.printLn("Text");

{
  Prelude.printLn("  append");

  let actual = Text.append("x", "y");
  let expected = "xy";

  assert(actual == expected);
};
