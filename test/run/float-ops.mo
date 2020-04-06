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

assert (Prim.floatCopySign(-9.7, 4.2) == 9.7);
assert (Prim.floatCopySign(9.7, -4.2) == -9.7);

assert (-9.7 < 4.2);
assert (-9.7 <= -9.7);
assert (-9.7 <= 4.2);

assert (-9.7 >= -9.7);
assert (9.7 >= -4.2);
assert (9.7 > -4.2);

assert (-9.7 != 4.2);

// Trigonometric
let pi = 3.141592653589793238;
assert (Prim.sin(0.0) == 0.0);
assert (Prim.sin(pi / 2.0) == 1.0);
assert (Prim.cos(0.0) == 1.0);
assert (Prim.cos(pi) == -1.0);

// Conversions
assert (Prim.floatToInt64(pi) == (3 : Int64));
assert (Prim.floatToInt64(-pi) == (-3 : Int64));

assert (Prim.int64ToFloat(42) == 42.0);
assert (Prim.int64ToFloat(-42) == -42.0);
