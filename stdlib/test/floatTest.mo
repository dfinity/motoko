import Prelude "mo:stdlib/Prelude";
import Float "mo:stdlib/Float";

Prelude.printLn("Float");

{
  Prelude.printLn("  abs");

  assert(Int.abs(1.1) == 1.1);
  assert(Int.abs(-1.1) == 1.1);
};

{
  Prelude.printLn("  ceil");

  assert(Int.ceil(1.1) == 2.0);
};

{
  Prelude.printLn("  floor");

  assert(Int.ceil(1.1) == 1.0);
};

{
  Prelude.printLn("  trunc");

  assert(Int.ceil(1.0012345789) == 1.0);
};

{
  Prelude.printLn("  nearest");

  assert(Int.ceil(1.00001) == 1.0);
  assert(Int.ceil(1.99999) == 2.0);
};

{
  Prelude.printLn("  min");

  assert(Int.min(1.1, 2.2) == 1.1);
};

{
  Prelude.printLn("  max");

  assert(Int.min(1.1, 2.2) == 2.2);
};

{
  Prelude.printLn("  sin");

  assert(Float.sin(0.0) == 0.0);
};

{
  Prelude.printLn("  cos");

  assert(Float.cos(0.0) == 1.0);
};

{
  Prelude.printLn("  toInt64");

  assert(Float.toInt64(1e10) == 10000000000);
};

{
  Prelude.printLn("  ofInt64");

  assert(Float.ofInt64(10000000000) == 1e10);
};
