import Array "array.as";
import Prelude "prelude.as";
import Text "text.as";

Prelude.printLn("Array");

{
  Prelude.printLn("  append");

  let actual = Array.append<Int>([ 1, 2, 3 ], [ 4, 5, 6 ]);
  let expected = [ 1, 2, 3, 4, 5, 6 ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  apply");

  let ask = func (x : Text) : Text {
    x # "?";
  };

  let exclaim = func (x : Text) : Text {
    x # "!";
  };

  let actual = Array.apply<Text, Text>([ ask, exclaim ], [ "good", "bad" ]);
  let expected = [ "good?", "bad?", "good!", "bad!" ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  bind");

  let purePlusOne = func (x : Int) : [Int] {
    [ x + 1 ];
  };

  let actual = Array.bind<Int, Int>([ 0, 1, 2 ], purePlusOne);
  let expected = [ 1, 2, 3 ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  find");

  type Element = {
    key : Text;
    value : Int;
  };

  let xs = [
    { key = "a"; value = 0; },
    { key = "b"; value = 1; },
    { key = "c"; value = 2; },
  ];

  let b : ?Element = Array.find<Element>(func (x : Element) : Bool {
    x.key == "b";
  }, xs);

  switch (b) {
    case (?element) {
      assert(element.key == "b" and element.value == 1);
    };
    case (_) {
      assert(false);
    };
  };
};

{
  Prelude.printLn("  foldl");

  let xs = [ "a", "b", "c" ];

  let actual = Array.foldl<Text, Text>(Text.append, "", xs);
  let expected = "abc";

  assert(actual == expected);
};

{
  Prelude.printLn("  foldr");

  let xs = [ "a", "b", "c" ];

  let actual = Array.foldr<Text, Text>(Text.append, "", xs);
  let expected = "abc";

  assert(actual == expected);
};

{
  Prelude.printLn("  freeze");

  var xs : [var Int] = [ var 1, 2, 3 ];

  let actual = Array.freeze<Int>(xs);
  let expected : [Int] = [ 1, 2, 3 ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  join");

  let xs = [ [ 1, 2, 3 ] ];

  let actual = Array.join<Int>(xs);
  let expected : [Int] = [ 1, 2, 3 ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  map");

  let isEven = func (x : Int) : Bool {
    x % 2 == 0;
  };

  let actual = Array.map<Int, Bool>(isEven, [ 1, 2, 3, 4, 5, 6 ]);
  let expected = [ false, true, false, true, false, true ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  pure");

  let actual = Array.pure<Int>(0);
  let expected = [0];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  thaw");

  let xs : [Int] = [ 1, 2, 3 ];

  let actual = Array.thaw<Int>(xs);
  var expected : [Int] = [ 1, 2, 3 ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};
