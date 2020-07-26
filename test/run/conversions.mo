import Prim "mo:prim";

// Nat <--> Word32

func n2w(n : Nat) : ?Word32 {
    let w = Prim.natToWord32 n;
    if (n == Prim.word32ToNat w)
       ?w else null
};

assert(Prim.natToWord32 0 == (0 : Word32));
assert(Prim.natToWord32 42 == (42 : Word32));
assert(Prim.natToWord32 65535 == (65535 : Word32)); // 2**16 - 1

assert(Prim.natToWord32 2147483647 == (2147483647 : Word32)); // 2**31 - 1
assert(Prim.natToWord32 2147483648 == (2147483648 : Word32)); // 2**31
assert(Prim.natToWord32 2147483649 == (2147483649 : Word32)); // 2**31 + 1
assert(Prim.natToWord32 4294967295 == (4294967295 : Word32)); // 2**32 - 1


assert(Prim.word32ToNat 0 == 0);
assert(Prim.word32ToNat 42 == 42);
assert(Prim.word32ToNat 2147483647 == 2147483647); // 2**31 - 1
assert(Prim.word32ToNat 4294967295 == 4294967295); // 2**32 - 1

func forall<T> (f : T -> (), l : [T]) = for (e in l.vals()) { f e };

{
    func roundtrip(n : Nat) = assert (Prim.word32ToNat (Prim.natToWord32 n) == n);
    forall<Nat>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);
};

{
    func roundtrip(w : Word32) = assert (Prim.natToWord32 (Prim.word32ToNat w) == w);
    forall<Word32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);



    func roundtripNat64(w : Word64) = assert (Prim.nat64ToWord64 (Prim.word64ToNat64 w) == w);
    forall<Word64>(roundtripNat64, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);

    func roundtripInt64(w : Word64) = assert (Prim.int64ToWord64 (Prim.word64ToInt64 w) == w);
    forall<Word64>(roundtripInt64, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);

    func roundtrip64i(w : Int) = assert (Prim.int64ToInt (Prim.intToInt64 w) == w);
    forall<Int>(roundtrip64i, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0x7FFFFFFFFFFFFFFF]);
    forall<Int>(roundtrip64i, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648, -9223372036854775808]);
    func roundtrip64n(w : Nat) = assert (Prim.nat64ToNat (Prim.natToNat64 w) == w);
    forall<Nat>(roundtrip64n, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);



    func roundtripNat32(w : Word32) = assert (Prim.nat32ToWord32 (Prim.word32ToNat32 w) == w);
    forall<Word32>(roundtripNat32, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    func roundtripInt32(w : Word32) = assert (Prim.int32ToWord32 (Prim.word32ToInt32 w) == w);
    forall<Word32>(roundtripInt32, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    func roundtrip32i(w : Int) = assert (Prim.int32ToInt (Prim.intToInt32 w) == w);
    forall<Int>(roundtrip32i, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF]);
    forall<Int>(roundtrip32i, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648]);
    func roundtrip32n(w : Nat) = assert (Prim.nat32ToNat (Prim.natToNat32 w) == w);
    forall<Nat>(roundtrip32n, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);


    func roundtripNat16(w : Word16) = assert (Prim.nat16ToWord16 (Prim.word16ToNat16 w) == w);
    forall<Word16>(roundtripNat16, [0, 10, 100, 1000, 10000, 0xFFFF]);

    func roundtripInt16(w : Word16) = assert (Prim.int16ToWord16 (Prim.word16ToInt16 w) == w);
    forall<Word16>(roundtripInt16, [0, 10, 100, 1000, 10000, 0xFFFF]);

    func roundtrip16i(w : Int) = assert (Prim.int16ToInt (Prim.intToInt16 w) == w);
    forall<Int>(roundtrip16i, [0, 10, 100, 1000, 10000, 0x7FFF]);
    forall<Int>(roundtrip16i, [-10, -100, -1000, -10000, -32768]);
    func roundtrip16n(w : Nat) = assert (Prim.nat16ToNat (Prim.natToNat16 w) == w);
    forall<Nat>(roundtrip16n, [0, 10, 100, 1000, 10000, 0xFFFF]);


    func roundtripNat8(w : Word8) = assert (Prim.nat8ToWord8 (Prim.word8ToNat8 w) == w);
    forall<Word8>(roundtripNat8, [0, 10, 100, 0xFF]);

    func roundtripInt8(w : Word8) = assert (Prim.int8ToWord8 (Prim.word8ToInt8 w) == w);
    forall<Word8>(roundtripInt8, [0, 10, 100, 0xFF]);

    func roundtrip8i(w : Int) = assert (Prim.int8ToInt (Prim.intToInt8 w) == w);
    forall<Int>(roundtrip8i, [0, 10, 100, 0x7F]);
    forall<Int>(roundtrip8i, [-10, -100, -128]);
    func roundtrip8n(w : Nat) = assert (Prim.nat8ToNat (Prim.natToNat8 w) == w);
    forall<Nat>(roundtrip8n, [0, 10, 100, 0xFF]);
};


// Int <--> Word32

func i2w(n : Nat) : ?Word32 {
    let w = Prim.intToWord32 n;
    if (n == Prim.int32ToInt (Prim.word32ToInt32(w)))
       ?w else null
};



assert(Prim.intToWord32 0 == (0 : Word32));
assert(Prim.intToWord32 42 == (42 : Word32));
assert(Prim.intToWord32 65535 == (65535 : Word32)); // 2**16 - 1

assert(Prim.intToWord32 (-42) == (-42/*!*/ : Word32));
assert(Prim.intToWord32 (-65535) == (-65535/*!*/ : Word32)); // - 2**16 + 1
assert(Prim.intToWord32 (-65536) == (-65536/*!*/ : Word32)); // - 2**16
assert(Prim.intToWord32 (-2147483648) == (-2147483648/*!*/ : Word32)); // - 2**31

assert(Prim.intToWord32 2147483647 == (2147483647 : Word32)); // 2**31 - 1
assert(Prim.intToWord32 2147483648 == (2147483648 : Word32)); // 2**31
assert(Prim.intToWord32 2147483649 == (2147483649 : Word32)); // 2**31 + 1
assert(Prim.intToWord32 4294967295 == (4294967295 : Word32)); // 2**32 - 1

func println(i : Int) { Prim.debugPrintInt(i) };

{
    func roundtrip(i : Int) = assert (Prim.int32ToInt (Prim.word32ToInt32 (Prim.intToWord32 i)) == i);
    forall<Int>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF]);

    forall<Int>(roundtrip, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648]);
};

{
    func roundtrip(w : Word32) = assert (Prim.intToWord32 (Prim.int32ToInt (Prim.word32ToInt32 w)) == w);
    forall<Word32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    /* non-canonical range for Word32 */
    forall<Word32>(roundtrip, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000]);
};




// Char <--> Word32

assert(Prim.charToWord32 '\u{00}' == (0 : Word32));
assert(Prim.charToWord32 '*' == (42 : Word32));
assert(Prim.charToWord32 'П' == (1055 : Word32));
assert(Prim.charToWord32 '\u{ffff}' == (65535 : Word32)); // 2**16 - 1
assert(Prim.charToWord32 '\u{10ffff}' == (0x10FFFF : Word32));

{
    func roundtrip(w : Word32) = assert (Prim.charToWord32 (Prim.word32ToChar w) == w);
    forall<Word32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 0x10FFFF]);  // largest code point
};


// Char <--> Text

assert(Prim.charToText 'П' == "П");
func snd((a : Word32, b : Char)) : Char = b;
assert(switch ("П".chars().next()) { case (?'П') true; case _ false });

// Nat <--> Word64

assert(Prim.natToWord64 0x0000000000000000 == (0x0000000000000000 : Word64));
assert(Prim.natToWord64 0x0000000000000001 == (0x0000000000000001 : Word64));
assert(Prim.natToWord64 0x7FFFFFFFFFFFFFFF == (0x7FFFFFFFFFFFFFFF : Word64));
assert(Prim.natToWord64 0x8000000000000000 == (0x8000000000000000 : Word64));
assert(Prim.natToWord64 0x8000000000000001 == (0x8000000000000001 : Word64));
assert(Prim.natToWord64 0xFFFFFFFFFFFFFFFF == (0xFFFFFFFFFFFFFFFF : Word64));

assert(Prim.word64ToNat 0x0000000000000000 == (0x0000000000000000 : Nat));
assert(Prim.word64ToNat 0x0000000000000001 == (0x0000000000000001 : Nat));
assert(Prim.word64ToNat 0x7FFFFFFFFFFFFFFF == (0x7FFFFFFFFFFFFFFF : Nat));
assert(Prim.word64ToNat 0x8000000000000000 == (0x8000000000000000 : Nat));
assert(Prim.word64ToNat 0x8000000000000001 == (0x8000000000000001 : Nat));
assert(Prim.word64ToNat 0xFFFFFFFFFFFFFFFF == (0xFFFFFFFFFFFFFFFF : Nat));

// Int <--> Word64

assert(Prim.intToWord64   0x0000000000000000 ==  (0x0000000000000000 : Word64));
assert(Prim.intToWord64   0x0000000000000001 ==  (0x0000000000000001 : Word64));
assert(Prim.intToWord64   0x7FFFFFFFFFFFFFFF ==  (0x7FFFFFFFFFFFFFFF : Word64));
assert(Prim.intToWord64 (-0x8000000000000000) == (0x8000000000000000 : Word64));
assert(Prim.intToWord64 (-0x7FFFFFFFFFFFFFFF) == (0x8000000000000001 : Word64));
assert(Prim.intToWord64 (-0x7FFFFFFFFFFFFFFE) == (0x8000000000000002 : Word64));
assert(Prim.intToWord64 (-0x0000000000000001) == (0xFFFFFFFFFFFFFFFF : Word64));

// Below conversions mainly test the interpreter's bignum arithmetics

// Int <--> Word8

assert(Prim.intToWord8 (2 ** 62) == (0 : Word8));
assert(Prim.intToWord8 (2 ** 62 + 1) == (1 : Word8));
assert(Prim.intToWord8 (2 ** 62 - 1) == (255 : Word8));
assert(Prim.intToWord8 (- 2 ** 62) == (0 : Word8));
assert(Prim.intToWord8 (- 2 ** 62 + 1) == (1 : Word8));
assert(Prim.intToWord8 (- 2 ** 62 - 1) == (255 : Word8));

assert(Prim.intToWord8 (2 ** 63) == (0 : Word8));
assert(Prim.intToWord8 (2 ** 63 + 1) == (1 : Word8));
assert(Prim.intToWord8 (2 ** 63 - 1) == (255 : Word8));
assert(Prim.intToWord8 (- 2 ** 63) == (0 : Word8));
assert(Prim.intToWord8 (- 2 ** 63 + 1) == (1 : Word8));
assert(Prim.intToWord8 (- 2 ** 63 - 1) == (255 : Word8));

assert(Prim.intToWord8 (2 ** 64) == (0 : Word8));
assert(Prim.intToWord8 (2 ** 64 + 1) == (1 : Word8));
assert(Prim.intToWord8 (2 ** 64 - 1) == (255 : Word8));
assert(Prim.intToWord8 (- 2 ** 64) == (0 : Word8));
assert(Prim.intToWord8 (- 2 ** 64 + 1) == (1 : Word8));
assert(Prim.intToWord8 (- 2 ** 64 - 1) == (255 : Word8));


// Nat <--> Word8

assert(Prim.natToWord8 (2 ** 64) == (0 : Word8));
assert(Prim.natToWord8 (2 ** 64 + 1) == (1 : Word8));
assert(Prim.natToWord8 (2 ** 64 - 1) == (255 : Word8));

// Int <--> Word16

assert(Prim.intToWord16 (2 ** 62) == (0 : Word16));
assert(Prim.intToWord16 (2 ** 62 + 1) == (1 : Word16));
assert(Prim.intToWord16 (2 ** 62 - 1) == (65535 : Word16));
assert(Prim.intToWord16 (- 2 ** 62) == (0 : Word16));
assert(Prim.intToWord16 (- 2 ** 62 + 1) == (1 : Word16));
assert(Prim.intToWord16 (- 2 ** 62 - 1) == (65535 : Word16));

assert(Prim.intToWord16 (2 ** 63) == (0 : Word16));
assert(Prim.intToWord16 (2 ** 63 + 1) == (1 : Word16));
assert(Prim.intToWord16 (2 ** 63 - 1) == (65535 : Word16));
assert(Prim.intToWord16 (- 2 ** 63) == (0 : Word16));
assert(Prim.intToWord16 (- 2 ** 63 + 1) == (1 : Word16));
assert(Prim.intToWord16 (- 2 ** 63 - 1) == (65535 : Word16));

assert(Prim.intToWord16 (2 ** 64) == (0 : Word16));
assert(Prim.intToWord16 (2 ** 64 + 1) == (1 : Word16));
assert(Prim.intToWord16 (2 ** 64 - 1) == (65535 : Word16));
assert(Prim.intToWord16 (- 2 ** 64) == (0 : Word16));
assert(Prim.intToWord16 (- 2 ** 64 + 1) == (1 : Word16));
assert(Prim.intToWord16 (- 2 ** 64 - 1) == (65535 : Word16));

// Nat <--> Word16

assert(Prim.natToWord16 (2 ** 62) == (0 : Word16));
assert(Prim.natToWord16 (2 ** 62 + 1) == (1 : Word16));
assert(Prim.natToWord16 (2 ** 62 - 1) == (65535 : Word16));

assert(Prim.natToWord16 (2 ** 63) == (0 : Word16));
assert(Prim.natToWord16 (2 ** 63 + 1) == (1 : Word16));
assert(Prim.natToWord16 (2 ** 63 - 1) == (65535 : Word16));

assert(Prim.natToWord16 (2 ** 64) == (0 : Word16));
assert(Prim.natToWord16 (2 ** 64 + 1) == (1 : Word16));
assert(Prim.natToWord16 (2 ** 64 - 1) == (65535 : Word16));

// Int <--> Word32

assert(Prim.intToWord32 (2 ** 62) == (0 : Word32));
assert(Prim.intToWord32 (2 ** 62 + 1) == (1 : Word32));
assert(Prim.intToWord32 (2 ** 62 - 1) == (4294967295 : Word32));
assert(Prim.intToWord32 (- 2 ** 62) == (0 : Word32));
assert(Prim.intToWord32 (- 2 ** 62 + 1) == (1 : Word32));
assert(Prim.intToWord32 (- 2 ** 62 - 1) == (4294967295 : Word32));

assert(Prim.intToWord32 (2 ** 63) == (0 : Word32));
assert(Prim.intToWord32 (2 ** 63 + 1) == (1 : Word32));
assert(Prim.intToWord32 (2 ** 63 - 1) == (4294967295 : Word32));
assert(Prim.intToWord32 (- 2 ** 63) == (0 : Word32));
assert(Prim.intToWord32 (- 2 ** 63 + 1) == (1 : Word32));
assert(Prim.intToWord32 (- 2 ** 63 - 1) == (4294967295 : Word32));

assert(Prim.intToWord32 (2 ** 64) == (0 : Word32));
assert(Prim.intToWord32 (2 ** 64 + 1) == (1 : Word32));
assert(Prim.intToWord32 (2 ** 64 - 1) == (4294967295 : Word32));
assert(Prim.intToWord32 (- 2 ** 64) == (0 : Word32));
assert(Prim.intToWord32 (- 2 ** 64 + 1) == (1 : Word32));
assert(Prim.intToWord32 (- 2 ** 64 - 1) == (4294967295 : Word32));

// Nat <--> Word32

assert(Prim.natToWord32 (2 ** 62) == (0 : Word32));
assert(Prim.natToWord32 (2 ** 62 + 1) == (1 : Word32));
assert(Prim.natToWord32 (2 ** 62 - 1) == (4294967295 : Word32));

assert(Prim.natToWord32 (2 ** 63) == (0 : Word32));
assert(Prim.natToWord32 (2 ** 63 + 1) == (1 : Word32));
assert(Prim.natToWord32 (2 ** 63 - 1) == (4294967295 : Word32));

assert(Prim.natToWord32 (2 ** 64) == (0 : Word32));
assert(Prim.natToWord32 (2 ** 64 + 1) == (1 : Word32));
assert(Prim.natToWord32 (2 ** 64 - 1) == (4294967295 : Word32));

