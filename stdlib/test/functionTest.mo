import Function "mo:stdlib/Function";
import Prelude "mo:stdlib/Prelude";
import Text "mo:stdlib/Text";

Prelude.printLn("Function");

{
  Prelude.printLn("  compose");

  func isEven(x : Int) : Bool { x % 2 == 0; };
  func not_(x : Bool) : Bool { not x; };
  let isOdd = Function.compose<Int, Bool, Bool>(not_, isEven);

  assert(isOdd(0) == false);
  assert(isOdd(1));
};

{
  Prelude.printLn("  const");

  assert(Function.const<Bool, Text>(true)("abc"));
  assert(Function.const<Bool, Text>(false)("abc") == false);
};

{
  Prelude.printLn("  const2");

  assert(Function.const2<Bool, Int, Text>(true)(0, "abc"));
  assert(Function.const2<Bool, Int, Text>(false)(0, "abc") == false);
};

{
  Prelude.printLn("  lift");

  let appendPair = Function.lift<Text, Text, Text>(Text.append);
  let pair = ("Hello, ", "World!");

  assert(appendPair(pair) == "Hello, World!");
};

{
  Prelude.printLn("  lower");

  func appendPair(pair : (Text, Text)) : Text {
    pair.0 # pair.1;
  };

  let append = Function.lower<Text, Text, Text>(appendPair);

  assert(append("Hello, ", "World!") == "Hello, World!");
};
