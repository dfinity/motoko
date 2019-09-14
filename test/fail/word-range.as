// Tests that the non-canonical range for words is accepted as literals,
// and conversely literals out of the range are rejected.

// accept these
let _ : Word8 = 255;
let _ : Word8 = 0xFF;
let _ : Word8 = +255;
let _ : Word8 = +0xFF;
let _ : Word8 = -128;
let _ : Word8 = -0x80;
let _ : Word16 = 65535;
let _ : Word16 = 0xFFFF;
let _ : Word16 = +65535;
let _ : Word16 = +0xFFFF;
let _ : Word16 = -32768;
let _ : Word16 = -0x8000;
let _ : Word32 = 4294967295;
let _ : Word32 = 0xFFFFFFFF;
let _ : Word32 = +4294967295;
let _ : Word32 = +0xFFFFFFFF;
let _ : Word32 = -2147483648;
let _ : Word32 = -0x80000000;
let _ : Word64 = 18_446_744_073_709_551_615;
let _ : Word64 = 0xFFFFFFFFFFFFFFFF;
let _ : Word64 = +18_446_744_073_709_551_615;
let _ : Word64 = +0xFFFFFFFFFFFFFFFF;
let _ : Word64 = -9_223_372_036_854_775_808;
let _ : Word64 = -0x8000000000000000;

// reject these
let _ : Word8 = 256;
let _ : Word8 = 0x100;
let _ : Word8 = +256;
let _ : Word8 = +0x100;
let _ : Word8 = -129;
let _ : Word8 = -0x81;
let _ : Word16 = 65536;
let _ : Word16 = 0x10000;
let _ : Word16 = +65536;
let _ : Word16 = +0x10000;
let _ : Word16 = -32769;
let _ : Word16 = -0x8001;
let _ : Word32 = 4294967296;
let _ : Word32 = 0x100000000;
let _ : Word32 = +4294967296;
let _ : Word32 = +0x100000000;
let _ : Word32 = -2147483649;
let _ : Word32 = -0x80000001;
let _ : Word64 = 18_446_744_073_709_551_616;
let _ : Word64 = 0x10000000000000000;
let _ : Word64 = +18_446_744_073_709_551_616;
let _ : Word64 = +0x10000000000000000;
let _ : Word64 = -9_223_372_036_854_775_809;
let _ : Word64 = -0x8000000000000001;
