import Iter "iter.mo";
import Prelude "prelude.mo";

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
