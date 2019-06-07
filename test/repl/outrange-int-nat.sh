#!/usr/bin/env bash
# Tests that the repl Int* and Nat* types properly trap
${ASC:-$(dirname "$BASH_SOURCE")/../../src/asc} -i <<__END__
intToInt8 0x7F;
intToInt8 0x80;
intToInt8 (-0x80);
intToInt8 (-0x81);

intToInt16 0x7FFF;
intToInt16 0x8000;
intToInt16 (-0x8000);
intToInt16 (-0x8001);

intToInt32 0x7FFFFFFF;
intToInt32 0x80000000;
intToInt32 (-0x80000000);
intToInt32 (-0x80000001);

intToInt64 0x7FFFFFFFFFFFFFFF;
intToInt64 0x8000000000000000;
intToInt64 (-0x8000000000000000);
intToInt64 (-0x8000000000000001);


natToNat8 0xFF;
natToNat8 0x100;

natToNat16 0xFFFF;
natToNat16 0x10000;

natToNat32 0xFFFFFFFF;
natToNat32 0x100000000;

natToNat64 0xFFFFFFFFFFFFFFFF;
natToNat64 0x10000000000000000;
__END__
