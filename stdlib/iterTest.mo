import Iter "iter.mo";
import Prelude "prelude.mo";

Debug.printLn("Iter");

{
  Debug.printLn("  forIn");

  let xs = [ "a", "b", "c", "d", "e", "f" ];

  var y = "";
  var z = 0;

  Iter.forIn<Text>(func (x : Text, i : Nat) {
    y := y # x;
    z += i;
  }, xs.vals());

  assert(y == "abcdef");
  assert(z == 15);
}
