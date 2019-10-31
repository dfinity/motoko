import Iter "iter.mo";
import Prelude "prelude.mo";

Prelude.printLn("Iter");

{
  Prelude.printLn("  for");

  let xs = [ "a", "b", "c", "d", "e", "f" ];

  var y = "";
  var z = 0;

  Iter.for<Text>(func (x : Text, i : Nat) {
    y := y # x;
    z += i;
  }, xs.vals());

  assert(y == "abcdef");
  assert(z == 15);
}
