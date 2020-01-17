import Prelude "prelude";
import Text "text";

Prelude.printLn("Text");

{
  Prelude.printLn("  append");

  let actual = Text.append("x", "y");
  let expected = "xy";

  assert(actual == expected);
};
