import Iter "mo:stdlib/Iter";
import List "mo:stdlib/List";
import Prelude "mo:stdlib/Prelude";

Prelude.printLn("Iter");

{
  Prelude.printLn("  forIn");

  let xs = [ "a", "b", "c", "d", "e", "f" ];

  var y = "";
  var z = 0;

  Iter.forIn<Text>(func (x : Text, i : Nat) {
    y := y # x;
    z += i;
  }, xs.vals());

  assert(y == "abcdef");
  assert(z == 15);
};

{
  Prelude.printLn("  map");

  let isEven = func (x : Int) : Bool {
    x % 2 == 0;
  };

  let _actual = Iter.map<Nat, Bool>(isEven, [ 1, 2, 3 ].vals());
  let actual = [var true, false, true];
  Iter.forIn<Bool>(func (x, i) { actual[i] := x; }, _actual);

  let expected = [false, true, false];

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  pure");

  let x = 1;
  let y = Iter.pure<Nat>(x);

  switch (y.next()) {
    case null { assert false; };
    case (?z) { assert (x == z); };
  };
};

{
  Prelude.printLn("  fromArray");

  let expected = [1, 2, 3];
  let _actual = Iter.fromArray<Nat>(expected);
  let actual = [var 0, 0, 0];

  Iter.forIn<Nat>(func (x, i) { actual[i] := x; }, _actual);

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  fromArrayMut");

  let expected = [var 1, 2, 3];
  let _actual = Iter.fromArrayMut<Nat>(expected);
  let actual = [var 0, 0, 0];

  Iter.forIn<Nat>(func (x, i) { actual[i] := x; }, _actual);

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  fromList");

  let list : List.List<Nat> = ?(1, ?(2, ?(3, List.nil<Nat>())));
  let _actual = Iter.fromList<Nat>(list);
  let actual = [var 0, 0, 0];
  let expected = [1, 2, 3];

  Iter.forIn<Nat>(func (x, i) { actual[i] := x; }, _actual);

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  toArray");

  let expected = [1, 2, 3];
  let actual = Iter.toArray<Nat>(expected.vals());

  assert (actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  toArrayMut");

  let expected = [var 1, 2, 3];
  let actual = Iter.toArrayMut<Nat>(expected.vals());

  assert (actual.len() == expected.len());

  for (i in actual.keys()) {
    assert(actual[i] == expected[i]);
  };
};

{
  Prelude.printLn("  toList");

  let expected : List.List<Nat> = ?(1, ?(2, ?(3, List.nil<Nat>())));
  let actual = Iter.toList<Nat>([1, 2, 3].vals());
  assert List.isEq<Nat>(expected, actual, func (x1, x2) { x1 == x2 });
};

{
  Prelude.printLn("  toListWithLength");

  let expected : {
    length : Nat;
    list : List.List<Nat>;
  } = {
    length = 3;
    list = ?(1, ?(2, ?(3, List.nil<Nat>())));
  };

  let actual = Iter.toListWithLength<Nat>([1, 2, 3].vals());

  assert (expected.length == actual.length);
  assert List.isEq<Nat>(expected.list, actual.list, func (x1, x2) { x1 == x2 });
};
