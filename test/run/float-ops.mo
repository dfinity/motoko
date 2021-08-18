import Prim "mo:⛔";

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

Prim.debugPrint(debug_show(+5.1 % +3.0));
Prim.debugPrint(debug_show(-5.1 % +3.0));
Prim.debugPrint(debug_show(+5.1 % -3.0));
Prim.debugPrint(debug_show(-5.1 % -3.0));

// Trigonometric

let pi = 3.141592653589793238;
let ninetyDegrees = (pi / 2.0);
assert (Prim.sin(0.0) == 0.0);
assert (Prim.sin(ninetyDegrees) == 1.0);
assert (Prim.cos(0.0) == 1.0);
assert (Prim.cos(pi) == -1.0);

let fortyFiveDegrees = (pi / 4.0);
assert (Prim.sin(fortyFiveDegrees) / Prim.cos(fortyFiveDegrees) == Prim.tan(fortyFiveDegrees));

let someRandomAngle = 0.42;
assert Prim.arcsin(Prim.sin(someRandomAngle)) == someRandomAngle;
assert Prim.floatAbs(Prim.arccos(Prim.cos(someRandomAngle)) - someRandomAngle) < 0.00000000000000006;
assert Prim.arctan(Prim.tan(someRandomAngle)) == someRandomAngle;

let sqrt2over2 : Float = Prim.floatSqrt(2) / 2;
assert Prim.floatAbs(Prim.arcsin(sqrt2over2) - fortyFiveDegrees) < 0.0000000000000002;
assert Prim.arcsin(1.0) == ninetyDegrees;

assert Prim.arccos(sqrt2over2) == fortyFiveDegrees;
assert Prim.arccos(1.0) == 0.0;

assert Prim.arctan(1.0) == fortyFiveDegrees;

assert Prim.arctan2(sqrt2over2, sqrt2over2) == fortyFiveDegrees;
assert Prim.arctan2(-sqrt2over2, sqrt2over2) == -fortyFiveDegrees;
assert Prim.arctan2(1, 0) == ninetyDegrees;
assert Prim.arctan2(0, 0) == 0.0;
assert Prim.arctan2(-1, 0) == -ninetyDegrees;

// Transcendental

assert Prim.exp(0.0) == 1.0;

let someRandomReal = 7.4225;
assert Prim.log(Prim.exp(someRandomReal)) == someRandomReal;
assert Prim.floatAbs(Prim.exp(Prim.log(someRandomReal)) - someRandomReal) < 0.000000000000001;

// Conversions
assert (Prim.floatToInt64(pi) == (3 : Int64));
assert (Prim.floatToInt64(-pi) == (-3 : Int64));

assert (Prim.int64ToFloat(42) == 42.0);
assert (Prim.int64ToFloat(-42) == -42.0);

Prim.debugPrint (debug_show Prim.floatToInt(-pi));
/*Prim.debugPrint (debug_show Prim.floatToInt(pi));
Prim.debugPrint (debug_show Prim.intToFloat(42));
Prim.debugPrint (debug_show Prim.intToFloat(-42));
Prim.debugPrint (debug_show Prim.intToFloat(-42_000_000_000));*/

assert (Prim.floatToInt(pi) == 3);
assert (Prim.floatToInt(-pi) == -3);

assert (Prim.intToFloat(42) == 42.0);
assert (Prim.intToFloat(-42) == -42.0);


assert (Prim.intToFloat(42_000_000_000) == 42000000000.0);
assert (Prim.intToFloat(-42_000_000_000) == -42000000000.0);
