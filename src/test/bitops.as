// Bit operators

func TestWord8(a : Word8, b : Word8) : () {
   let not1 = ^ a;
   let not2 = (^ a) : Word8;
   let or1 = a | b;
   let or2 = (a | b) : Word8;
   let and1 = a & b;
   let and2 = (a & b) : Word8;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word8;
   let shiftl = a << b;
   let shiftl = a << b : Word8;
   let shiftr = a >> b;
   let shiftr = a >> b : Word8;
   let rotl = a <<> b;
   let rotl = a <<> b : Word8;
   let rotr = a <>> b;
   let rotr = a <>> b : Word8;
};


func TestWord16(a : Word16, b : Word16) : () {
   let not1 = ^ a;
   let not2 = (^ a) : Word16;
   let or1 = a | b;
   let or2 = (a | b) : Word16;
   let and1 = a & b;
   let and2 = (a & b) : Word16;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word16;
   let shiftl = a << b;
   let shiftl = a << b : Word16;
   let shiftr = a >> b;
   let shiftr = a >> b : Word16;
   let rotl = a <<> b;
   let rotl = a <<> b : Word16;
   let rotr = a <>> b;
   let rotr = a <>> b : Word16;
};

func TestWord32(a : Word32, b : Word32) : () {
   let not1 = ^ a;
   let not2 = (^ a) : Word32;
   let or1 = a | b;
   let or2 = (a | b) : Word32;
   let and1 = a & b;
   let and2 = (a & b) : Word32;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word32;
   let shiftl = a << b;
   let shiftl = a << b : Word32;
   let shiftr = a >> b;
   let shiftr = a >> b : Word32;
   let rotl = a <<> b;
   let rotl = a <<> b : Word32;
   let rotr = a <>> b;
   let rotr = a <>> b : Word32;
};

func TestWord64(a : Word64, b : Word64) : () {
   let not1 = ^ a;
   let not2 = (^ a) : Word64;
   let or1 = a | b;
   let or2 = (a | b) : Word64;
   let and1 = a & b;
   let and2 = (a & b) : Word64;
   let xor = a ^ b;
   let xor2 = (a ^ b) : Word64;
   let shiftl = a << b;
   let shiftl = a << b : Word64;
   let shiftr = a >> b;
   let shiftr = a >> b : Word64;
   let rotl = a <<> b;
   let rotl = a <<> b : Word64;
   let rotr = a <>> b;
   let rotr = a <>> b : Word64;
};
