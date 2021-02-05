// Tests that only the canonical range for word-like types is accepted as
// literals, literals out of the range are rejected.

// accept these
let _ : Nat8 = 255;
let _ : Nat8 = 0xFF;
let _ : Nat8 = 0;
let _ : Int8 = 127;
let _ : Int8 = 0x7F;
let _ : Int8 = +127;
let _ : Int8 = +0x7F;
let _ : Int8 = -128;
let _ : Int8 = -0x80;

let _ : Nat16 = 65535;
let _ : Nat16 = 0xFFFF;
let _ : Nat16 = 0;
let _ : Int16 = 32767;
let _ : Int16 = 0x7FFF;
let _ : Int16 = +32767;
let _ : Int16 = +0x7FFF;
let _ : Int16 = -32768;
let _ : Int16 = -0x8000;

let _ : Nat32 = 4294967295;
let _ : Nat32 = 0xFFFFFFFF;
let _ : Nat32 = 0;
let _ : Int32 = 2147483647;
let _ : Int32 = 0x7FFFFFFF;
let _ : Int32 = +2147483647;
let _ : Int32 = +0x7FFFFFFF;
let _ : Int32 = -2147483648;
let _ : Int32 = -0x80000000;

let _ : Nat64 = 18_446_744_073_709_551_615;
let _ : Nat64 = 0xFFFFFFFFFFFFFFFF;
let _ : Nat64 = 0;
let _ : Int64 = 9_223_372_036_854_775_807;
let _ : Int64 = 0x7FFFFFFFFFFFFFFF;
let _ : Int64 = +9_223_372_036_854_775_807;
let _ : Int64 = +0x7FFFFFFFFFFFFFFF;
let _ : Int64 = -9_223_372_036_854_775_808;
let _ : Int64 = -0x8000000000000000;

// reject these
let _ : Nat8 = 256;
let _ : Nat8 = 0x100;
let _ : Int8 = 128;
let _ : Int8 = 0x80;
let _ : Int8 = +128;
let _ : Int8 = +0x80;
let _ : Int8 = -129;
let _ : Int8 = -0x7f;

let _ : Nat16 = 65536;
let _ : Nat16 = 0x10000;
let _ : Int16 = 32768;
let _ : Int16 = 0x8000;
let _ : Int16 = +32768;
let _ : Int16 = +0x8000;
let _ : Int16 = -32769;
let _ : Int16 = -0x7fff;

let _ : Nat32 = 4294967296;
let _ : Nat32 = 0x100000000;
let _ : Int32 = 2147483648;
let _ : Int32 = 0x80000000;
let _ : Int32 = +2147483648;
let _ : Int32 = +0x80000000;
let _ : Int32 = -2147483649;
let _ : Int32 = -0x7fffffff;

let _ : Nat64 = 18_446_744_074_709_551_615;
let _ : Nat64 = 0x0000000000000000;
let _ : Int64 = 9_223_372_036_854_775_808;
let _ : Int64 = 0x8000000000000000;
let _ : Int64 = +9_223_372_036_855_775_808;
let _ : Int64 = +0x8000000000000000;
let _ : Int64 = -9_223_372_036_854_775_809;
let _ : Int64 = -0x8000000000000000;
