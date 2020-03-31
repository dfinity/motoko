import Array "mo:stdlib/Array";
import Prelude "mo:stdlib/Prelude";
import Text "mo:stdlib/Text";

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
  Prelude.printLn("  enumerate");

  let xs = [ "a", "b", "c" ];
  let ys = Array.enumerate<Text>(xs);

  assert(xs.len() == ys.len());

  assert(ys[0].0 == xs[0]);
  assert(ys[0].1 == 0);

  assert(ys[1].0 == xs[1]);
  assert(ys[1].1 == 1);

  assert(ys[2].0 == xs[2]);
  assert(ys[2].1 == 2);
};

{
  Prelude.printLn("  filter");

  let isEven = func (x : Int) : Bool {
    x % 2 == 0;
  };

  let actual = Array.filter<Nat>(isEven, [ 1, 2, 3, 4, 5, 6 ]);
  let expected = [ 2, 4, 6 ];

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
  Prelude.printLn("  mapWithIndex");

  let isEven = func (x : Int) : Bool {
    x % 2 == 0;
  };

  let xs = [ 1, 2, 3, 4, 5, 6 ];

  let actual = Array.mapWithIndex<Int, (Bool, Bool)>(
    func (value : Int, index : Nat) : (Bool, Bool) {
      (isEven value, isEven index)
    },
    xs
  );

  let expected = [
    (false, true),
    (true, false),
    (false, true),
    (true, false),
    (false, true),
    (true, false),
  ];

  assert(actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i].0 == expected[i].0);
    assert(actual[i].1 == expected[i].1);
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

{
  Prelude.printLn("  tabulateVar");

  // regression test for (fixed) issues in base cases, where func was called too often:

  let test0 = Array.tabulateVar<Nat>(0, func (i:Nat) { assert(false); 0 });
  let test1 = Array.tabulateVar<Nat>(1, func (i:Nat) { assert(i < 1); 0 });
  let test2 = Array.tabulateVar<Nat>(2, func (i:Nat) { assert(i < 2); 0 });
  let test3 = Array.tabulateVar<Nat>(3, func (i:Nat) { assert(i < 3); 0 });

};
