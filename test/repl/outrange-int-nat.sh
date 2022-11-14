#!/usr/bin/env bash
# Tests that the repl Int* and Nat* types properly trap
moc -i <<__END__
import Prim "mo:⛔";
Prim.intToInt8 0x7F;
Prim.intToInt8 0x80;
Prim.intToInt8 (-0x80);
Prim.intToInt8 (-0x81);

Prim.intToInt16 0x7FFF;
Prim.intToInt16 0x8000;
Prim.intToInt16 (-0x8000);
Prim.intToInt16 (-0x8001);

Prim.intToInt32 0x7FFFFFFF;
Prim.intToInt32 0x80000000;
Prim.intToInt32 (-0x80000000);
Prim.intToInt32 (-0x80000001);

Prim.intToInt64 0x7FFFFFFFFFFFFFFF;
Prim.intToInt64 0x8000000000000000;
Prim.intToInt64 (-0x8000000000000000);
Prim.intToInt64 (-0x8000000000000001);


Prim.natToNat8 0xFF;
Prim.natToNat8 0x100;

Prim.natToNat16 0xFFFF;
Prim.natToNat16 0x10000;

Prim.natToNat32 0xFFFFFFFF;
Prim.natToNat32 0x100000000;

Prim.natToNat64 0xFFFFFFFFFFFFFFFF;
Prim.natToNat64 0x10000000000000000;


-127 : Int8;
-0x7F : Int8;
-32767 : Int16;
-0x7FFF : Int16;
-2147483647 : Int32;
-0x7FFFFFFF : Int32;
-9223372036854775807 : Int64;
-0x7FFFFFFFFFFFFFFF : Int64;

__END__
