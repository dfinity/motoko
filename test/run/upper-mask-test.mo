func upperMask(a : Word8) : Bool = a & (0x80 : Word8) != (0 : Word8);

func upperMaskNeg(a : Word8) : Bool = a & (0x80 : Word8) == (0 : Word8);

assert upperMask 0xF7;
assert upperMaskNeg 0x7F;

func upperMaskFlipped(a : Word8) : Bool = 0x80 : Word8 & a != (0 : Word8);
func upperMaskNegFlipped(a : Word8) : Bool = 0x80 : Word8 & a == (0 : Word8);

assert upperMaskFlipped 0xF7;
assert upperMaskNegFlipped 0x7F;

func upperBitGe(a : Word8) : Bool = a >= (0x80 : Word8);
func upperBitGt(a : Word8) : Bool = a > (0x7F : Word8);

assert upperBitGe 0xF7;
assert upperBitGt 0xF7;
