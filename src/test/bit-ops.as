// Bit operators

func testWord8(a : Word8, b : Word8) {
   let not1 = ^ a;
   let not2 = (^ a) : Word8;
   let or1 = a | b;
   let or2 = (a | b) : Word8;
   let and1 = a & b;
   let and2 = (a & b) : Word8;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word8;
   let shl1 = a << b;
   let shl2 = a << b : Word8;
   let shr1 = a >> b;
   let shr2 = a >> b : Word8;
   let rotl1 = a <<> b;
   let rotl2 = a <<> b : Word8;
   let rotr1 = a <>> b;
   let rotr2 = a <>> b : Word8;
};


func testWord16(a : Word16, b : Word16) {
   let not1 = ^ a;
   let not2 = (^ a) : Word16;
   let or1 = a | b;
   let or2 = (a | b) : Word16;
   let and1 = a & b;
   let and2 = (a & b) : Word16;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word16;
   let shl1 = a << b;
   let shl2 = a << b : Word16;
   let shr1 = a >> b;
   let shr2 = a >> b : Word16;
   let rotl1 = a <<> b;
   let rotl2 = a <<> b : Word16;
   let rotr1 = a <>> b;
   let rotr2 = a <>> b : Word16;
};

func testWord32(a : Word32, b : Word32) {
   let not1 = ^ a;
   let not2 = (^ a) : Word32;
   let or1 = a | b;
   let or2 = (a | b) : Word32;
   let and1 = a & b;
   let and2 = (a & b) : Word32;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word32;
   let shl1 = a << b;
   let shl2 = a << b : Word32;
   let shr1 = a >> b;
   let shr2 = a >> b : Word32;
   let rotl1 = a <<> b;
   let rotl2 = a <<> b : Word32;
   let rotr1 = a <>> b;
   let rotr2 = a <>> b : Word32;
};

func testWord64(a : Word64, b : Word64) {
   let not1 = ^ a;
   let not2 = (^ a) : Word64;
   let or1 = a | b;
   let or2 = (a | b) : Word64;
   let and1 = a & b;
   let and2 = (a & b) : Word64;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word64;
   let shl1 = a << b;
   let shl2 = a << b : Word64;
   let shr1 = a >> b;
   let shr2 = a >> b : Word64;
   let rotl1 = a <<> b;
   let rotl2 = a <<> b : Word64;
   let rotr1 = a <>> b;
   let rotr2 = a <>> b : Word64;
};
