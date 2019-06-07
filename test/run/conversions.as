// Nat <--> Word32

func n2w(n : Nat) : ?Word32 {
    let w = natToWord32 n;
    if (n == word32ToNat w)
       ?w else null
};

assert(natToWord32 0 == (0 : Word32));
assert(natToWord32 42 == (42 : Word32));
assert(natToWord32 65535 == (65535 : Word32)); // 2**16 - 1

assert(natToWord32 2147483647 == (2147483647 : Word32)); // 2**31 - 1
assert(natToWord32 2147483648 == (2147483648 : Word32)); // 2**31
assert(natToWord32 2147483649 == (2147483649 : Word32)); // 2**31 + 1
assert(natToWord32 4294967295 == (4294967295 : Word32)); // 2**32 - 1


assert(word32ToNat 0 == 0);
assert(word32ToNat 42 == 42);
assert(word32ToNat 2147483647 == 2147483647); // 2**31 - 1
assert(word32ToNat 4294967295 == 4294967295); // 2**32 - 1

func forall<T> (f : T -> (), l : [T]) = for (e in l.vals()) { f e };

{
    func roundtrip(n : Nat) = assert (word32ToNat (natToWord32 n) == n);
    forall<Nat>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);
};

{
    func roundtrip(w : Word32) = assert (natToWord32 (word32ToNat w) == w);
    forall<Word32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);



    func roundtripNat64(w : Word64) = assert (nat64ToWord64 (word64ToNat64 w) == w);
    forall<Word64>(roundtripNat64, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);

    func roundtripInt64(w : Word64) = assert (int64ToWord64 (word64ToInt64 w) == w);
    forall<Word64>(roundtripInt64, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF/*, 0xFFFFFFFFFFFFFFFF*/]);
    // BUG: roundtripInt64 0xFFFFFFFFFFFFFFFF;

    func roundtrip64i(w : Int) = assert (int64ToInt (intToInt64 w) == w);
    forall<Int>(roundtrip64i, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0x7FFFFFFFFFFFFFFF]);
    forall<Int>(roundtrip64i, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648, -9223372036854775808]);
    func roundtrip64n(w : Nat) = assert (nat64ToNat (natToNat64 w) == w);
    forall<Nat>(roundtrip64n, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);



    func roundtripNat32(w : Word32) = assert (nat32ToWord32 (word32ToNat32 w) == w);
    forall<Word32>(roundtripNat32, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    func roundtripInt32(w : Word32) = assert (int32ToWord32 (word32ToInt32 w) == w);
    forall<Word32>(roundtripInt32, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    func roundtrip32i(w : Int) = assert (int32ToInt (intToInt32 w) == w);
    forall<Int>(roundtrip32i, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF]);
    forall<Int>(roundtrip32i, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648]);
    func roundtrip32n(w : Nat) = assert (nat32ToNat (natToNat32 w) == w);
    forall<Nat>(roundtrip32n, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);


    func roundtripNat16(w : Word16) = assert (nat16ToWord16 (word16ToNat16 w) == w);
    forall<Word16>(roundtripNat16, [0, 10, 100, 1000, 10000, 0xFFFF]);

    func roundtripInt16(w : Word16) = assert (int16ToWord16 (word16ToInt16 w) == w);
    forall<Word16>(roundtripInt16, [0, 10, 100, 1000, 10000, 0xFFFF]);

    func roundtrip16i(w : Int) = assert (int16ToInt (intToInt16 w) == w);
    forall<Int>(roundtrip16i, [0, 10, 100, 1000, 10000, 0x7FFF]);
    forall<Int>(roundtrip16i, [-10, -100, -1000, -10000, -32768]);
    func roundtrip16n(w : Nat) = assert (nat16ToNat (natToNat16 w) == w);
    forall<Nat>(roundtrip16n, [0, 10, 100, 1000, 10000, 0xFFFF]);


    func roundtripNat8(w : Word8) = assert (nat8ToWord8 (word8ToNat8 w) == w);
    forall<Word8>(roundtripNat8, [0, 10, 100, 0xFF]);

    func roundtripInt8(w : Word8) = assert (int8ToWord8 (word8ToInt8 w) == w);
    forall<Word8>(roundtripInt8, [0, 10, 100, 0xFF]);

    func roundtrip8i(w : Int) = assert (int8ToInt (intToInt8 w) == w);
    forall<Int>(roundtrip8i, [0, 10, 100, 0x7F]);
    forall<Int>(roundtrip8i, [-10, -100, -128]);
    func roundtrip8n(w : Nat) = assert (nat8ToNat (natToNat8 w) == w);
    forall<Nat>(roundtrip8n, [0, 10, 100, 0xFF]);
};


// Int <--> Word32

func i2w(n : Nat) : ?Word32 {
    let w = intToWord32 n;
    if (n == word32ToInt w)
       ?w else null
};



assert(intToWord32 0 == (0 : Word32));
assert(intToWord32 42 == (42 : Word32));
assert(intToWord32 65535 == (65535 : Word32)); // 2**16 - 1

assert(intToWord32 (-42) == (-42/*!*/ : Word32));
assert(intToWord32 (-65535) == (-65535/*!*/ : Word32)); // - 2**16 + 1
assert(intToWord32 (-65536) == (-65536/*!*/ : Word32)); // - 2**16
assert(intToWord32 (-2147483648) == (-2147483648/*!*/ : Word32)); // - 2**31

assert(intToWord32 2147483647 == (2147483647 : Word32)); // 2**31 - 1
assert(intToWord32 2147483648 == (2147483648 : Word32)); // 2**31
assert(intToWord32 2147483649 == (2147483649 : Word32)); // 2**31 + 1
assert(intToWord32 4294967295 == (4294967295 : Word32)); // 2**32 - 1

func println(i : Int) { printInt(i); print "\n"; };

println(word32ToInt (-2147483648)); // -2**31
println(word32ToInt (-2147483647)); // -2**31 + 1
println(word32ToInt (-42));
println(word32ToInt (-1));
println(word32ToInt 0);
println(word32ToInt 42);
println(word32ToInt 2147483647); // 2**31 - 1
println(word32ToInt 2147483648); // == (-2147483648) // 2**31
println(word32ToInt 4294967294); // == (-2) // 2**32 - 2
println(word32ToInt 4294967295); // == (-1) // 2**32 - 1

{
    func roundtrip(i : Int) = assert (word32ToInt (intToWord32 i) == i);
    forall<Int>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF]);

    forall<Int>(roundtrip, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648]);
};

{
    func roundtrip(w : Word32) = assert (intToWord32 (word32ToInt w) == w);
    forall<Word32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    /*!*/
    forall<Word32>(roundtrip, [(-10), (-100), (-1000), (-10000), (-100000), (-1000000), (-10000000), (-100000000), (-1000000000)]);
};




// Char <--> Word32

assert(charToWord32 '\u{00}' == (0 : Word32));
assert(charToWord32 '*' == (42 : Word32));
assert(charToWord32 'П' == (1055 : Word32));
assert(charToWord32 '\u{ffff}' == (65535 : Word32)); // 2**16 - 1
assert(charToWord32 '\u{10ffff}' == (0x10FFFF : Word32));

{
    func roundtrip(w : Word32) = assert (charToWord32 (word32ToChar w) == w);
    forall<Word32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 0x10FFFF]);  // largest code point
};


// Char <--> Text

assert(charToText 'П' == "П");
func snd((a : Word32, b : Char)) : Char = b;
assert(snd (decodeUTF8 "П") =='П');

// Nat <--> Word64

assert(natToWord64 0x0000000000000000 == (0x0000000000000000 : Word64));
assert(natToWord64 0x0000000000000001 == (0x0000000000000001 : Word64));
assert(natToWord64 0x7FFFFFFFFFFFFFFF == (0x7FFFFFFFFFFFFFFF : Word64));
assert(natToWord64 0x8000000000000000 == (0x8000000000000000 : Word64));
assert(natToWord64 0x8000000000000001 == (0x8000000000000001 : Word64));
assert(natToWord64 0xFFFFFFFFFFFFFFFF == (0xFFFFFFFFFFFFFFFF : Word64));

assert(word64ToNat 0x0000000000000000 == (0x0000000000000000 : Nat));
assert(word64ToNat 0x0000000000000001 == (0x0000000000000001 : Nat));
assert(word64ToNat 0x7FFFFFFFFFFFFFFF == (0x7FFFFFFFFFFFFFFF : Nat));
assert(word64ToNat 0x8000000000000000 == (0x8000000000000000 : Nat));
assert(word64ToNat 0x8000000000000001 == (0x8000000000000001 : Nat));
assert(word64ToNat 0xFFFFFFFFFFFFFFFF == (0xFFFFFFFFFFFFFFFF : Nat));

// Int <--> Word64

assert(intToWord64   0x0000000000000000 ==  (0x0000000000000000 : Word64));
assert(intToWord64   0x0000000000000001 ==  (0x0000000000000001 : Word64));
assert(intToWord64   0x7FFFFFFFFFFFFFFF ==  (0x7FFFFFFFFFFFFFFF : Word64));
assert(intToWord64 (-0x8000000000000000) == (0x8000000000000000 : Word64));
assert(intToWord64 (-0x7FFFFFFFFFFFFFFF) == (0x8000000000000001 : Word64));
assert(intToWord64 (-0x7FFFFFFFFFFFFFFE) == (0x8000000000000002 : Word64));
assert(intToWord64 (-0x0000000000000001) == (0xFFFFFFFFFFFFFFFF : Word64));

assert(word64ToInt (0x0000000000000000 : Word64) ==  0x0000000000000000);
assert(word64ToInt (0x0000000000000001 : Word64) ==  0x0000000000000001);
assert(word64ToInt (0x7FFFFFFFFFFFFFFF : Word64) ==  0x7FFFFFFFFFFFFFFF);
assert(word64ToInt (0x8000000000000000 : Word64) == -0x8000000000000000);
assert(word64ToInt (0x8000000000000001 : Word64) == -0x7FFFFFFFFFFFFFFF);
assert(word64ToInt (0x8000000000000002 : Word64) == -0x7FFFFFFFFFFFFFFE);
assert(word64ToInt (0xFFFFFFFFFFFFFFFF : Word64) == -0x0000000000000001);

// Below conversions mainly test the interpreter's bignum arithmetics

// Int <--> Word8

assert(intToWord8 (2 ** 62) == (0 : Word8));
assert(intToWord8 (2 ** 62 + 1) == (1 : Word8));
assert(intToWord8 (2 ** 62 - 1) == (255 : Word8));
assert(intToWord8 (- 2 ** 62) == (0 : Word8));
assert(intToWord8 (- 2 ** 62 + 1) == (1 : Word8));
assert(intToWord8 (- 2 ** 62 - 1) == (255 : Word8));

assert(intToWord8 (2 ** 63) == (0 : Word8));
assert(intToWord8 (2 ** 63 + 1) == (1 : Word8));
assert(intToWord8 (2 ** 63 - 1) == (255 : Word8));
assert(intToWord8 (- 2 ** 63) == (0 : Word8));
assert(intToWord8 (- 2 ** 63 + 1) == (1 : Word8));
assert(intToWord8 (- 2 ** 63 - 1) == (255 : Word8));

assert(intToWord8 (2 ** 64) == (0 : Word8));
assert(intToWord8 (2 ** 64 + 1) == (1 : Word8));
assert(intToWord8 (2 ** 64 - 1) == (255 : Word8));
assert(intToWord8 (- 2 ** 64) == (0 : Word8));
assert(intToWord8 (- 2 ** 64 + 1) == (1 : Word8));
assert(intToWord8 (- 2 ** 64 - 1) == (255 : Word8));


// Nat <--> Word8

assert(natToWord8 (2 ** 64) == (0 : Word8));
assert(natToWord8 (2 ** 64 + 1) == (1 : Word8));
assert(natToWord8 (2 ** 64 - 1) == (255 : Word8));

// Int <--> Word16

assert(intToWord16 (2 ** 62) == (0 : Word16));
assert(intToWord16 (2 ** 62 + 1) == (1 : Word16));
assert(intToWord16 (2 ** 62 - 1) == (65535 : Word16));
assert(intToWord16 (- 2 ** 62) == (0 : Word16));
assert(intToWord16 (- 2 ** 62 + 1) == (1 : Word16));
assert(intToWord16 (- 2 ** 62 - 1) == (65535 : Word16));

assert(intToWord16 (2 ** 63) == (0 : Word16));
assert(intToWord16 (2 ** 63 + 1) == (1 : Word16));
assert(intToWord16 (2 ** 63 - 1) == (65535 : Word16));
assert(intToWord16 (- 2 ** 63) == (0 : Word16));
assert(intToWord16 (- 2 ** 63 + 1) == (1 : Word16));
assert(intToWord16 (- 2 ** 63 - 1) == (65535 : Word16));

assert(intToWord16 (2 ** 64) == (0 : Word16));
assert(intToWord16 (2 ** 64 + 1) == (1 : Word16));
assert(intToWord16 (2 ** 64 - 1) == (65535 : Word16));
assert(intToWord16 (- 2 ** 64) == (0 : Word16));
assert(intToWord16 (- 2 ** 64 + 1) == (1 : Word16));
assert(intToWord16 (- 2 ** 64 - 1) == (65535 : Word16));

// Nat <--> Word16

assert(natToWord16 (2 ** 62) == (0 : Word16));
assert(natToWord16 (2 ** 62 + 1) == (1 : Word16));
assert(natToWord16 (2 ** 62 - 1) == (65535 : Word16));

assert(natToWord16 (2 ** 63) == (0 : Word16));
assert(natToWord16 (2 ** 63 + 1) == (1 : Word16));
assert(natToWord16 (2 ** 63 - 1) == (65535 : Word16));

assert(natToWord16 (2 ** 64) == (0 : Word16));
assert(natToWord16 (2 ** 64 + 1) == (1 : Word16));
assert(natToWord16 (2 ** 64 - 1) == (65535 : Word16));

// Int <--> Word32

assert(intToWord32 (2 ** 62) == (0 : Word32));
assert(intToWord32 (2 ** 62 + 1) == (1 : Word32));
assert(intToWord32 (2 ** 62 - 1) == (4294967295 : Word32));
assert(intToWord32 (- 2 ** 62) == (0 : Word32));
assert(intToWord32 (- 2 ** 62 + 1) == (1 : Word32));
assert(intToWord32 (- 2 ** 62 - 1) == (4294967295 : Word32));

assert(intToWord32 (2 ** 63) == (0 : Word32));
assert(intToWord32 (2 ** 63 + 1) == (1 : Word32));
assert(intToWord32 (2 ** 63 - 1) == (4294967295 : Word32));
assert(intToWord32 (- 2 ** 63) == (0 : Word32));
assert(intToWord32 (- 2 ** 63 + 1) == (1 : Word32));
assert(intToWord32 (- 2 ** 63 - 1) == (4294967295 : Word32));

assert(intToWord32 (2 ** 64) == (0 : Word32));
assert(intToWord32 (2 ** 64 + 1) == (1 : Word32));
assert(intToWord32 (2 ** 64 - 1) == (4294967295 : Word32));
assert(intToWord32 (- 2 ** 64) == (0 : Word32));
assert(intToWord32 (- 2 ** 64 + 1) == (1 : Word32));
assert(intToWord32 (- 2 ** 64 - 1) == (4294967295 : Word32));

// Nat <--> Word32

assert(natToWord32 (2 ** 62) == (0 : Word32));
assert(natToWord32 (2 ** 62 + 1) == (1 : Word32));
assert(natToWord32 (2 ** 62 - 1) == (4294967295 : Word32));

assert(natToWord32 (2 ** 63) == (0 : Word32));
assert(natToWord32 (2 ** 63 + 1) == (1 : Word32));
assert(natToWord32 (2 ** 63 - 1) == (4294967295 : Word32));

assert(natToWord32 (2 ** 64) == (0 : Word32));
assert(natToWord32 (2 ** 64 + 1) == (1 : Word32));
assert(natToWord32 (2 ** 64 - 1) == (4294967295 : Word32));

