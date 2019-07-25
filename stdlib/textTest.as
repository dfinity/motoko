import Prelude "prelude.as";
import Text "text.as";

Prelude.printLn("Text");

{
  Prelude.printLn("  append");

  let actual = Text.append("x", "y");
  let expected = "xy";

  assert(actual == expected);
};
