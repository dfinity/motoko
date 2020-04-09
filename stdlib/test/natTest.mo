import Prelude "mo:stdlib/Prelude";
import Nat "mo:stdlib/Nat";

Prelude.printLn("Nat");

{
  Prelude.printLn("  add");

  assert(Nat.add(1, Nat.add(2, 3)) == Nat.add(1, Nat.add(2, 3)));
  assert(Nat.add(0, 1) == 1);
  assert(1 == Nat.add(1, 0));
  assert(Nat.add(0, 1) == Nat.add(1, 0));
  assert(Nat.add(1, 2) == Nat.add(2, 1));
};

{
  Prelude.printLn("  toText");

  assert(Nat.toText(0) == "0");
  assert(Nat.toText(1234) == "1234");
};
