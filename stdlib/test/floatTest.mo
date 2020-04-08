import Prelude "mo:stdlib/Prelude";
import Float "mo:stdlib/Float";

Prelude.printLn("Float");

{
  Prelude.printLn("  abs");

  assert(Float.abs(1.1) == 1.1);
  assert(Float.abs(-1.1) == 1.1);
};

{
  Prelude.printLn("  ceil");

  assert(Float.ceil(1.1) == 2.0);
};

{
  Prelude.printLn("  floor");

  assert(Float.floor(1.1) == 1.0);
};

{
  Prelude.printLn("  trunc");

  assert(Float.trunc(1.0012345789) == 1.0);
};

{
  Prelude.printLn("  nearest");

  assert(Float.nearest(1.00001) == 1.0);
  assert(Float.nearest(1.99999) == 2.0);
};

{
  Prelude.printLn("  min");

  assert(Float.min(1.1, 2.2) == 1.1);
};

{
  Prelude.printLn("  max");

  assert(Float.max(1.1, 2.2) == 2.2);
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
  Prelude.printLn("  toFloat64");

  assert(Float.toInt64(1e10) == (10000000000 : Int64));
  assert(Float.toInt64(-1e10) == (-10000000000 : Int64));
};

{
  Prelude.printLn("  ofFloat64");

  assert(Float.ofInt64(10000000000) == 1e10);
  assert(Float.ofInt64(-10000000000) == -1e10);
};
