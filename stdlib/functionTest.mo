import Function "function.mo";
import Prelude "prelude.mo";
import Text "text.mo";

Prelude.printLn("Function");

{
  Prelude.printLn("  curry");

  func appendPair(pair : (Text, Text)) : Text {
    pair.0 # pair.1;
  };

  let append = Function.curry<Text, Text, Text>(appendPair);

  assert(append("Hello, ", "World!") == "Hello, World!");
};

{
  Prelude.printLn("  uncurry");

  let appendPair = Function.uncurry<Text, Text, Text>(Text.append);
  let pair = ("Hello, ", "World!");

  assert(appendPair(pair) == "Hello, World!");
};
