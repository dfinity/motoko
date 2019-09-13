// Tests that the non-canonical range for words is accepted as literals,
// and conversely literals out of the range are rejected.

// accept these
let _ : Word8 = 255;
let _ : Word8 = +255;
let _ : Word8 = -128;
let _ : Word16 = 65535;
let _ : Word16 = +65535;
let _ : Word16 = -32768;
let _ : Word32 = 4294967295;
let _ : Word32 = +4294967295;
let _ : Word32 = -2147483648;
let _ : Word64 = 18_446_744_073_709_551_615;
let _ : Word64 = +18_446_744_073_709_551_615;
let _ : Word64 = -9_223_372_036_854_775_808;

// reject these
let _ : Word8 = 256;
let _ : Word8 = +256;
let _ : Word8 = -129;
let _ : Word16 = 65536;
let _ : Word16 = +65536;
let _ : Word16 = -32769;
let _ : Word32 = 4294967296;
let _ : Word32 = +4294967296;
let _ : Word32 = -2147483649;
let _ : Word64 = 18_446_744_073_709_551_616;
let _ : Word64 = +18_446_744_073_709_551_616;
let _ : Word64 = -9_223_372_036_854_775_809;
