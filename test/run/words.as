// CHECK: func $start

func printBit(a : Bool) { print(if a "set" else "clear"); print "\n" };


func checkpointAlpha() {};
func checkpointBravo() {};
func checkpointCharlie() {};
func checkpointDelta() {};
func checkpointEcho() {};
func checkpointFoxtrot() {};
func checkpointGolf() {};
func checkpointHotel() {};
func checkpointIndia() {};
func checkpointJuliett() {};

// Word64 operations
{
    func printW64ln(w : Word64) { printInt(word64ToNat w);  print " "; printInt(word64ToInt w); print "\n" };

    let a : Word64 = 4567;
    let b : Word64 = 7;
    let c : Word64 = 8912765;
    let d : Word64 = -15;
    let e : Word64 = 20000;

// CHECK: local.get $c
// CHECK-NOT: call $box_i64
// CHECK: call $printW64ln
    printW64ln(+c);
    printW64ln(-c);
    printW64ln(^c);
    printW64ln(a + c);
    printW64ln(c - a);

// CHECK: call $checkpointAlpha
    checkpointAlpha();
// This is a native Wasm i64 multiplication, there should be no shift involved!
// CHECK-NOT: i64.shr_u
// CHECK: call $printW64ln
    printW64ln(a * b);

    printW64ln(a / b);
    printW64ln(c % a);
    printW64ln(a ** 2);

    printW64ln(a & c);
    printW64ln(a | c);
    printW64ln(a ^ c);
    printW64ln(a << b);
    printW64ln(a >> b);
    printW64ln(-5225319197819536385 >> 4); // 0b1011011101111011111011111101111111011111111011111111101111111111L == -5225319197819536385L --> 826339054743125951L
    printW64ln(d +>> 3);
    printW64ln(-5225319197819536385 +>> 4); // 0b1011011101111011111011111101111111011111111011111111101111111111L == -5225319197819536385L --> -326582449863721025L
    printW64ln(c <<> b);
    printW64ln(c <>> b);
    printW64ln(popcntWord64 d); // -15 = 0xfffffffffffffff1 = 0b1111_..._1111_1111_0001 (population = 61)
    printW64ln(clzWord64 e); // 20000 = 0x0000000000004e20 (leading zeros = 49)
    printW64ln(ctzWord64 e); // 20000 = 0x0000000000004e20 (trailing zeros = 5)
    printBit(btstWord64(e, 5 : Word64)); // 20000 = 0x0000000000004e20 (result = true)
    printBit(btstWord64(e, 63 : Word64)); // 20000 = 0x0000000000004e20 (result = false)
    printBit(btstWord64(e, 69 : Word64)); // 20000 = 0x0000000000004e20 (mod 64, result = true)

    assert (3 : Word64 ** (4 : Word64) == (81 : Word64));
    assert (3 : Word64 ** (7 : Word64) == (2187 : Word64));
    assert (3 : Word64 ** (14 : Word64) == (4782969 : Word64));
    assert (3 : Word64 ** (20 : Word64) == (3486784401 : Word64));
};



// Word32 operations
{
    func printW32ln(w : Word32) { printInt(word32ToNat w);  print " "; printInt(word32ToInt w); print "\n" };

    let a : Word32 = 4567;
    let b : Word32 = 7;
    let c : Word32 = 8912765;
    let d : Word32 = -15;
    let e : Word32 = 20000;

// CHECK: call $checkpointBravo
    checkpointBravo();
// CHECK: local.get $c
// CHECK-NOT: call $box_i32
// CHECK: call $printW32ln
    printW32ln(+c);
    printW32ln(-c);
    printW32ln(^c);
    printW32ln(a + c);
    printW32ln(c - a);

// CHECK: call $checkpointCharlie
    checkpointCharlie();
// This is a native Wasm i32 multiplication, there should be no shift involved!
// CHECK-NOT: i32.shr_u
// CHECK: call $printW32ln
    printW32ln(a * b);
    printW32ln(a / b);
    printW32ln(c % a);
    printW32ln(a ** 2);

    printW32ln(a & c);
    printW32ln(a | c);
    printW32ln(a ^ c);
    printW32ln(a << b);
    printW32ln(a >> b);
    printW32ln(-1216614433 >> 4); // 0b10110111011110111110111111011111l == -1216614433l --> 192397053l
    printW32ln(d +>> 3);
    printW32ln(-1216614433 +>> 4); // 0b10110111011110111110111111011111l == -1216614433l --> -76038403
    printW32ln(c <<> b);
    printW32ln(c <>> b);
    printW32ln(popcntWord32 d); // -15 = 0xfffffff1 = 0b1111_1111_1111_1111_1111_1111_1111_0001 (population = 29)
    printW32ln(clzWord32 e); // 20000 = 0x00004e20 (leading zeros = 17)
    printW32ln(ctzWord32 e); // 20000 = 0x00004e20 (trailing zeros = 5)
    printBit(btstWord32(e, 5 : Word32)); // 20000 = 0x00004e20 (result = true)
    printBit(btstWord32(e, 31 : Word32)); // 20000 = 0x00004e20 (result = false)
    printBit(btstWord32(e, 37 : Word32)); // 20000 = 0x00004e20 (mod 32, result = true)

    assert (3 : Word32 ** (4 : Word32) == (81 : Word32));
    assert (3 : Word32 ** (7 : Word32) == (2187 : Word32));
    assert (3 : Word32 ** (14 : Word32) == (4782969 : Word32));
    assert (3 : Word32 ** (20 : Word32) == (3486784401 : Word32));
};

// Word16 operations
{
    func printW16ln(w : Word16) { printInt(word16ToNat w); print " "; printInt(word16ToInt w); print "\n" };

    let a : Word16 = 4567;
    let b : Word16 = 7;
    let c : Word16 = 55734;
    let d : Word16 = -15;
    let e : Word16 = 20000;


    printW16ln(+c);
    printW16ln(-c);
    printW16ln(^c);
    printW16ln(a + c);
    printW16ln(c - a);

// CHECK: call $checkpointDelta
    checkpointDelta();
// CHECK: local.get $a
// This is not a native Wasm i32 multiplication, we need to shift one of the args left by 16 bits!
// CHECK-NEXT: local.get $b
// CHECK-NEXT: i32.const 16
// CHECK-NEXT: i32.shr_u
// CHECK-NEXT: i32.mul
// CHECK-NEXT: call $printW16ln
    printW16ln(a * b);
    printW16ln(a / b);
    printW16ln(c % a);
    printW16ln(a ** 2);

    printW16ln(a & c);
    printW16ln(a | c);
    printW16ln(a ^ c);
    printW16ln(a << b);

// CHECK: call $checkpointEcho
   checkpointEcho();
// CHECK: local.get $b
// This is not a native Wasm i32 left shift, we need to shift the second arg left by 16 bits and clamp it to 4 bits!
// CHECK-NEXT: i32.const 16
// CHECK-NEXT: i32.shr_u
// CHECK-NEXT: i32.const 15
// CHECK-NEXT: i32.and
// CHECK-NEXT: i32.shr_u
// Then the result must be sanitised.
// CHECK-NEXT: i32.const -65536
// CHECK-NEXT: i32.and
// CHECK-NEXT: call $printW16ln
    printW16ln(a >> b);
    printW16ln(d >> 3); // -15 = 0xfff1 = 0b1111_1111_1111_0001 (shifted = 0b0001_1111_1111_1110 = 8190)
    printW16ln(d +>> 3); // -15 = 0xfff1 = 0b1111_1111_1111_0001 (shifted = 0b1111_1111_1111_1110 = -2)

// CHECK: call $checkpointFoxtrot
   checkpointFoxtrot();
// CHECK: local.get $b
// CHECK-NEXT: call $rotl<Word16>
// CHECK-NEXT: call $printW16ln
    printW16ln(c <<> b);

// CHECK: call $checkpointGolf
   checkpointGolf();
// CHECK: local.get $b
// CHECK-NEXT: call $rotr<Word16>
// CHECK-NEXT: call $printW16ln
    printW16ln(c <>> b);
    printW16ln(popcntWord16 d); // -15 = 0xfff1 = 0b1111_1111_1111_0001 (population = 13)
    printW16ln(clzWord16 e); // 20000 = 0x4e20 (leading zeros = 1)
    printW16ln(ctzWord16 e); // 20000 = 0x4e20 (trailing zeros = 5)
    printBit(btstWord16(e, 5 : Word16)); // 20000 = 0x4e20 (result = true)
    printBit(btstWord16(e, 15 : Word16)); // 20000 = 0x4e20 (result = false)
    printBit(btstWord16(e, 21 : Word16)); // 20000 = 0x4e20 (mod 16, result = true)


    assert (3 : Word16 ** (0 : Word16) == (1 : Word16));
    assert (3 : Word16 ** (1 : Word16) == (3 : Word16));
    assert (3 : Word16 ** (4 : Word16) == (81 : Word16));
    assert (3 : Word16 ** (7 : Word16) == (2187 : Word16));
};

// Word8 operations
{
    func printW8ln(w : Word8) { printInt(word8ToNat w); print " "; printInt(word8ToInt w); print "\n" };

    let a : Word8 = 67;
    let b : Word8 = 7;
    let c : Word8 = 34;
    let d : Word8 = -15;
    let e : Word8 = 200;


    printW8ln(+c);
    printW8ln(-c);
    printW8ln(^c);
    printW8ln(a + c);
    printW8ln(c - a);
// CHECK: call $checkpointHotel
    checkpointHotel();
// CHECK: local.get $b
// This is not a native Wasm i32 multiplication, we need to shift one of the args left by 24 bits!
// CHECK-NEXT: i32.const 24
// CHECK-NEXT: i32.shr_u
// CHECK-NEXT: i32.mul
// CHECK-NEXT: call $printW8ln
    printW8ln(a * b);
    printW8ln(a / b);
    printW8ln(c % a);
    printW8ln(a ** 2);

    printW8ln(a & c);
    printW8ln(a | c);
    printW8ln(a ^ c);
    printW8ln(a << b);

// CHECK: call $checkpointIndia
    checkpointIndia();
// CHECK: local.get $b
// This is not a native Wasm i32 left shift, we need to shift the second arg left by 24 bits and clamp it to 3 bits!
// CHECK-NEXT: i32.const 24
// CHECK-NEXT: i32.shr_u
// CHECK-NEXT: i32.const 7
// CHECK-NEXT: i32.and
// CHECK-NEXT: i32.shr_u
// Then the result must be sanitised.
// CHECK-NEXT: i32.const -16777216
// CHECK-NEXT: i32.and
// CHECK-NEXT: call $printW8ln
    printW8ln(a >> b);
    printW8ln(d >> 3); // -15 = 0xf1 = 0b1111_0001 (shifted = 0b0001_1110 = 30)
    printW8ln(d +>> 3); // -15 = 0xf1 = 0b1111_0001 (shifted = 0b1111_1110 = -2)

// CHECK: call $checkpointJuliett
    checkpointJuliett();
// CHECK: local.get $b
// CHECK-NEXT: call $rotl<Word8>
// CHECK-NEXT: call $printW8ln
    printW8ln(c <<> b);
// CHECK: local.get $b
// CHECK-NEXT: call $rotr<Word8>
// CHECK-NEXT: call $printW8ln
    printW8ln(c <>> b);
    printW8ln(popcntWord8 d); // -15 = 0xf1 = 0b1111_0001 (population = 5)
    printW8ln(clzWord8 e); // 200 = 0xC8 (leading zeros = 0)
    printW8ln(ctzWord8 e); // 200 = 0xC8 (trailing zeros = 3)
    printBit(btstWord8(e, 3 : Word8)); // 200 = 0xC8 (result = true)
    printBit(btstWord8(e, 5 : Word8)); // 200 = 0xC8 (result = false)
    printBit(btstWord8(e, 11 : Word8)); // 200 = 0xC8 (mod 8, result = true)

    assert (3 : Word8 ** (0 : Word8) == (1 : Word8));
    assert (3 : Word8 ** (3 : Word8) == (27 : Word8));
    assert (3 : Word8 ** (4 : Word8) == (81 : Word8));
    assert (3 : Word8 ** (5 : Word8) == (243 : Word8));
};
