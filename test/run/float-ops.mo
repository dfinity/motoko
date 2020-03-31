import Prim "mo:prim";

assert (Prim.floatAbs(9.7) == 9.7);
assert (Prim.floatAbs(-9.7) == 9.7);

assert (Prim.floatSqrt(0.0) == 0.0);
assert (Prim.floatSqrt(25.0) == 5.0);

assert (Prim.floatCeil(9.7) == 10.0);
assert (Prim.floatCeil(-9.7) == -9.0);

assert (Prim.floatFloor(9.7) == 9.0);
assert (Prim.floatFloor(-9.7) == -10.0);

assert (Prim.floatTrunc(9.7) == 9.0);
assert (Prim.floatTrunc(-9.7) == -9.0);

assert (Prim.floatNearest(9.7) == 10.0);
assert (Prim.floatNearest(-9.7) == -10.0);
assert (Prim.floatNearest(9.4) == 9.0);
assert (Prim.floatNearest(-9.4) == -9.0);

assert (Prim.floatMin(9.7, 4.2) == 4.2);
assert (Prim.floatMin(-9.7, 4.2) == -9.7);

assert (Prim.floatMax(9.7, 4.2) == 9.7);
assert (Prim.floatMax(-9.7, 4.2) == 4.2);

assert (Prim.floatCopysign(-9.7, 4.2) == 9.7);
assert (Prim.floatCopysign(9.7, -4.2) == -9.7);
