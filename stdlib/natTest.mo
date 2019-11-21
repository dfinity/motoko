import Debug "debug.mo";
import Nat "nat.mo";

Debug.printLn("Nat");

{
  Debug.printLn("  add");

  assert(Nat.add(1, Nat.add(2, 3)) == Nat.add(1, Nat.add(2, 3)));
  assert(Nat.add(0, 1) == 1);
  assert(1 == Nat.add(1, 0));
  assert(Nat.add(0, 1) == Nat.add(1, 0));
  assert(Nat.add(1, 2) == Nat.add(2, 1));
};

{
  Debug.printLn("  toText");

  assert(Nat.toText(0) == "0");
  assert(Nat.toText(1234) == "1234");
};
