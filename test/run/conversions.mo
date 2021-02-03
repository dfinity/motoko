import Prim "mo:prim";

// Nat <--> Nat32

func n2w(n : Nat) : ?Nat32 {
    let w = Prim.natToNat32 n;
    if (n == Prim.nat32ToNat w)
       ?w else null
};

assert(Prim.natToNat32 0 == (0 : Nat32));
assert(Prim.natToNat32 42 == (42 : Nat32));
assert(Prim.natToNat32 65535 == (65535 : Nat32)); // 2**16 - 1

assert(Prim.natToNat32 2147483647 == (2147483647 : Nat32)); // 2**31 - 1
assert(Prim.natToNat32 2147483648 == (2147483648 : Nat32)); // 2**31
assert(Prim.natToNat32 2147483649 == (2147483649 : Nat32)); // 2**31 + 1
assert(Prim.natToNat32 4294967295 == (4294967295 : Nat32)); // 2**32 - 1


assert(Prim.nat32ToNat 0 == 0);
assert(Prim.nat32ToNat 42 == 42);
assert(Prim.nat32ToNat 2147483647 == 2147483647); // 2**31 - 1
assert(Prim.nat32ToNat 4294967295 == 4294967295); // 2**32 - 1

func forall<T> (f : T -> (), l : [T]) = for (e in l.vals()) { f e };

do {
    func roundtrip(n : Nat) = assert (Prim.nat32ToNat (Prim.natToNat32 n) == n);
    forall<Nat>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);
};

do {
    func roundtrip(w : Nat32) = assert (Prim.natToNat32 (Prim.nat32ToNat w) == w);
    forall<Nat32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);



    func roundtripNat64(w : Nat64) = assert (Prim.nat64ToNat64 (Prim.nat64ToNat64 w) == w);
    forall<Nat64>(roundtripNat64, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);

    func roundtripInt64(w : Nat64) = assert (Prim.int64ToNat64 (Prim.nat64ToInt64 w) == w);
    forall<Nat64>(roundtripInt64, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);

    func roundtrip64i(w : Int) = assert (Prim.int64ToInt (Prim.intToInt64 w) == w);
    forall<Int>(roundtrip64i, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0x7FFFFFFFFFFFFFFF]);
    forall<Int>(roundtrip64i, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648, -9223372036854775808]);
    func roundtrip64n(w : Nat) = assert (Prim.nat64ToNat (Prim.natToNat64 w) == w);
    forall<Nat>(roundtrip64n, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFFFFFFFF]);



    func roundtripNat32(w : Nat32) = assert (Prim.nat32ToNat32 (Prim.nat32ToNat32 w) == w);
    forall<Nat32>(roundtripNat32, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    func roundtripInt32(w : Nat32) = assert (Prim.int32ToNat32 (Prim.nat32ToInt32 w) == w);
    forall<Nat32>(roundtripInt32, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    func roundtrip32i(w : Int) = assert (Prim.int32ToInt (Prim.intToInt32 w) == w);
    forall<Int>(roundtrip32i, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF]);
    forall<Int>(roundtrip32i, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648]);
    func roundtrip32n(w : Nat) = assert (Prim.nat32ToNat (Prim.natToNat32 w) == w);
    forall<Nat>(roundtrip32n, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);


    func roundtripNat16(w : Nat16) = assert (Prim.nat16ToNat16 (Prim.nat16ToNat16 w) == w);
    forall<Nat16>(roundtripNat16, [0, 10, 100, 1000, 10000, 0xFFFF]);

    func roundtripInt16(w : Nat16) = assert (Prim.int16ToNat16 (Prim.nat16ToInt16 w) == w);
    forall<Nat16>(roundtripInt16, [0, 10, 100, 1000, 10000, 0xFFFF]);

    func roundtrip16i(w : Int) = assert (Prim.int16ToInt (Prim.intToInt16 w) == w);
    forall<Int>(roundtrip16i, [0, 10, 100, 1000, 10000, 0x7FFF]);
    forall<Int>(roundtrip16i, [-10, -100, -1000, -10000, -32768]);
    func roundtrip16n(w : Nat) = assert (Prim.nat16ToNat (Prim.natToNat16 w) == w);
    forall<Nat>(roundtrip16n, [0, 10, 100, 1000, 10000, 0xFFFF]);


    func roundtripNat8(w : Nat8) = assert (Prim.nat8ToNat8 (Prim.nat8ToNat8 w) == w);
    forall<Nat8>(roundtripNat8, [0, 10, 100, 0xFF]);

    func roundtripInt8(w : Nat8) = assert (Prim.int8ToNat8 (Prim.nat8ToInt8 w) == w);
    forall<Nat8>(roundtripInt8, [0, 10, 100, 0xFF]);

    func roundtrip8i(w : Int) = assert (Prim.int8ToInt (Prim.intToInt8 w) == w);
    forall<Int>(roundtrip8i, [0, 10, 100, 0x7F]);
    forall<Int>(roundtrip8i, [-10, -100, -128]);
    func roundtrip8n(w : Nat) = assert (Prim.nat8ToNat (Prim.natToNat8 w) == w);
    forall<Nat>(roundtrip8n, [0, 10, 100, 0xFF]);
};


// Int <--> Int32

func i2w(n : Int) : ?Int32 {
    let w = Prim.intToInt32 n;
    if (n == Prim.int32ToInt (Prim.nat32ToInt32(w)))
       ?w else null
};



assert(Prim.intToInt32 0 == (0 : Int32));
assert(Prim.intToInt32 42 == (42 : Int32));
assert(Prim.intToInt32 65535 == (65535 : Int32)); // 2**16 - 1

assert(Prim.intToInt32 (-42) == (-42/*!*/ : Int32));
assert(Prim.intToInt32 (-65535) == (-65535/*!*/ : Int32)); // - 2**16 + 1
assert(Prim.intToInt32 (-65536) == (-65536/*!*/ : Int32)); // - 2**16
assert(Prim.intToInt32 (-2147483648) == (-2147483648/*!*/ : Int32)); // - 2**31

assert(Prim.intToInt32 2147483647 == (2147483647 : Int32)); // 2**31 - 1
assert(Prim.intToInt32 2147483648 == (2147483648 : Int32)); // 2**31
assert(Prim.intToInt32 2147483649 == (2147483649 : Int32)); // 2**31 + 1
assert(Prim.intToInt32 4294967295 == (4294967295 : Int32)); // 2**32 - 1

func println(i : Int) { Prim.debugPrintInt(i) };

do {
    func roundtrip(i : Int) = assert (Prim.int32ToInt (Prim.nat32ToInt32 (Prim.intToInt32 i)) == i);
    forall<Int>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF]);

    forall<Int>(roundtrip, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000, -2147483648]);
};

do {
    func roundtrip(w : Int32) = assert (Prim.intToInt32 (Prim.int32ToInt (Prim.nat32ToInt32 w)) == w);
    forall<Int32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 0x7FFFFFFF, 0xFFFFFFFF]);

    /* non-canonical range for Int32 */
    forall<Int32>(roundtrip, [-10, -100, -1000, -10000, -100000, -1000000, -10000000, -100000000, -1000000000]);
};




// Char <--> Nat32

assert(Prim.charToNat32 '\u{00}' == (0 : Nat32));
assert(Prim.charToNat32 '*' == (42 : Nat32));
assert(Prim.charToNat32 'П' == (1055 : Nat32));
assert(Prim.charToNat32 '\u{ffff}' == (65535 : Nat32)); // 2**16 - 1
assert(Prim.charToNat32 '\u{10ffff}' == (0x10FFFF : Nat32));

do {
    func roundtrip(w : Nat32) = assert (Prim.charToNat32 (Prim.nat32ToChar w) == w);
    forall<Nat32>(roundtrip, [0, 10, 100, 1000, 10000, 100000, 1000000, 0x10FFFF]);  // largest code point
};


// Char <--> Text

assert(Prim.charToText 'П' == "П");
func snd((a : Nat32, b : Char)) : Char = b;
assert(switch ("П".chars().next()) { case (?'П') true; case _ false });

// Nat <--> Nat64

assert(Prim.natToNat64 0x0000000000000000 == (0x0000000000000000 : Nat64));
assert(Prim.natToNat64 0x0000000000000001 == (0x0000000000000001 : Nat64));
assert(Prim.natToNat64 0x7FFFFFFFFFFFFFFF == (0x7FFFFFFFFFFFFFFF : Nat64));
assert(Prim.natToNat64 0x8000000000000000 == (0x8000000000000000 : Nat64));
assert(Prim.natToNat64 0x8000000000000001 == (0x8000000000000001 : Nat64));
assert(Prim.natToNat64 0xFFFFFFFFFFFFFFFF == (0xFFFFFFFFFFFFFFFF : Nat64));

assert(Prim.nat64ToNat 0x0000000000000000 == (0x0000000000000000 : Nat));
assert(Prim.nat64ToNat 0x0000000000000001 == (0x0000000000000001 : Nat));
assert(Prim.nat64ToNat 0x7FFFFFFFFFFFFFFF == (0x7FFFFFFFFFFFFFFF : Nat));
assert(Prim.nat64ToNat 0x8000000000000000 == (0x8000000000000000 : Nat));
assert(Prim.nat64ToNat 0x8000000000000001 == (0x8000000000000001 : Nat));
assert(Prim.nat64ToNat 0xFFFFFFFFFFFFFFFF == (0xFFFFFFFFFFFFFFFF : Nat));

// Int <--> Int64

assert(Prim.intToInt64   0x0000000000000000 ==  (0x0000000000000000 : Int64));
assert(Prim.intToInt64   0x0000000000000001 ==  (0x0000000000000001 : Int64));
assert(Prim.intToInt64   0x7FFFFFFFFFFFFFFF ==  (0x7FFFFFFFFFFFFFFF : Int64));
assert(Prim.intToInt64 (-0x8000000000000000) == (0x8000000000000000 : Int64));
assert(Prim.intToInt64 (-0x7FFFFFFFFFFFFFFF) == (0x8000000000000001 : Int64));
assert(Prim.intToInt64 (-0x7FFFFFFFFFFFFFFE) == (0x8000000000000002 : Int64));
assert(Prim.intToInt64 (-0x0000000000000001) == (0xFFFFFFFFFFFFFFFF : Int64));

// Below conversions mainly test the interpreter's bignum arithmetics

// Int <--> Int8

assert(Prim.intToInt8 (2 ** 62) == (0 : Int8));
assert(Prim.intToInt8 (2 ** 62 + 1) == (1 : Int8));
assert(Prim.intToInt8 (2 ** 62 - 1) == (255 : Int8));
assert(Prim.intToInt8 (- 2 ** 62) == (0 : Int8));
assert(Prim.intToInt8 (- 2 ** 62 + 1) == (1 : Int8));
assert(Prim.intToInt8 (- 2 ** 62 - 1) == (255 : Int8));

assert(Prim.intToInt8 (2 ** 63) == (0 : Int8));
assert(Prim.intToInt8 (2 ** 63 + 1) == (1 : Int8));
assert(Prim.intToInt8 (2 ** 63 - 1) == (255 : Int8));
assert(Prim.intToInt8 (- 2 ** 63) == (0 : Int8));
assert(Prim.intToInt8 (- 2 ** 63 + 1) == (1 : Int8));
assert(Prim.intToInt8 (- 2 ** 63 - 1) == (255 : Int8));

assert(Prim.intToInt8 (2 ** 64) == (0 : Int8));
assert(Prim.intToInt8 (2 ** 64 + 1) == (1 : Int8));
assert(Prim.intToInt8 (2 ** 64 - 1) == (255 : Int8));
assert(Prim.intToInt8 (- 2 ** 64) == (0 : Int8));
assert(Prim.intToInt8 (- 2 ** 64 + 1) == (1 : Int8));
assert(Prim.intToInt8 (- 2 ** 64 - 1) == (255 : Int8));


// Nat <--> Nat8

assert(Prim.natToNat8 (2 ** 64) == (0 : Nat8));
assert(Prim.natToNat8 (2 ** 64 + 1) == (1 : Nat8));
assert(Prim.natToNat8 (2 ** 64 - 1) == (255 : Nat8));

// Int <--> Int16

assert(Prim.intToInt16 (2 ** 62) == (0 : Int16));
assert(Prim.intToInt16 (2 ** 62 + 1) == (1 : Int16));
assert(Prim.intToInt16 (2 ** 62 - 1) == (65535 : Int16));
assert(Prim.intToInt16 (- 2 ** 62) == (0 : Int16));
assert(Prim.intToInt16 (- 2 ** 62 + 1) == (1 : Int16));
assert(Prim.intToInt16 (- 2 ** 62 - 1) == (65535 : Int16));

assert(Prim.intToInt16 (2 ** 63) == (0 : Int16));
assert(Prim.intToInt16 (2 ** 63 + 1) == (1 : Int16));
assert(Prim.intToInt16 (2 ** 63 - 1) == (65535 : Int16));
assert(Prim.intToInt16 (- 2 ** 63) == (0 : Int16));
assert(Prim.intToInt16 (- 2 ** 63 + 1) == (1 : Int16));
assert(Prim.intToInt16 (- 2 ** 63 - 1) == (65535 : Int16));

assert(Prim.intToInt16 (2 ** 64) == (0 : Int16));
assert(Prim.intToInt16 (2 ** 64 + 1) == (1 : Int16));
assert(Prim.intToInt16 (2 ** 64 - 1) == (65535 : Int16));
assert(Prim.intToInt16 (- 2 ** 64) == (0 : Int16));
assert(Prim.intToInt16 (- 2 ** 64 + 1) == (1 : Int16));
assert(Prim.intToInt16 (- 2 ** 64 - 1) == (65535 : Int16));

// Nat <--> Nat16

assert(Prim.natToNat16 (2 ** 62) == (0 : Nat16));
assert(Prim.natToNat16 (2 ** 62 + 1) == (1 : Nat16));
assert(Prim.natToNat16 (2 ** 62 - 1) == (65535 : Nat16));

assert(Prim.natToNat16 (2 ** 63) == (0 : Nat16));
assert(Prim.natToNat16 (2 ** 63 + 1) == (1 : Nat16));
assert(Prim.natToNat16 (2 ** 63 - 1) == (65535 : Nat16));

assert(Prim.natToNat16 (2 ** 64) == (0 : Nat16));
assert(Prim.natToNat16 (2 ** 64 + 1) == (1 : Nat16));
assert(Prim.natToNat16 (2 ** 64 - 1) == (65535 : Nat16));

// Int <--> Int32

assert(Prim.intToInt32 (2 ** 62) == (0 : Int32));
assert(Prim.intToInt32 (2 ** 62 + 1) == (1 : Int32));
assert(Prim.intToInt32 (2 ** 62 - 1) == (4294967295 : Int32));
assert(Prim.intToInt32 (- 2 ** 62) == (0 : Int32));
assert(Prim.intToInt32 (- 2 ** 62 + 1) == (1 : Int32));
assert(Prim.intToInt32 (- 2 ** 62 - 1) == (4294967295 : Int32));

assert(Prim.intToInt32 (2 ** 63) == (0 : Int32));
assert(Prim.intToInt32 (2 ** 63 + 1) == (1 : Int32));
assert(Prim.intToInt32 (2 ** 63 - 1) == (4294967295 : Int32));
assert(Prim.intToInt32 (- 2 ** 63) == (0 : Int32));
assert(Prim.intToInt32 (- 2 ** 63 + 1) == (1 : Int32));
assert(Prim.intToInt32 (- 2 ** 63 - 1) == (4294967295 : Int32));

assert(Prim.intToInt32 (2 ** 64) == (0 : Int32));
assert(Prim.intToInt32 (2 ** 64 + 1) == (1 : Int32));
assert(Prim.intToInt32 (2 ** 64 - 1) == (4294967295 : Int32));
assert(Prim.intToInt32 (- 2 ** 64) == (0 : Int32));
assert(Prim.intToInt32 (- 2 ** 64 + 1) == (1 : Int32));
assert(Prim.intToInt32 (- 2 ** 64 - 1) == (4294967295 : Int32));

// Nat <--> Nat32

assert(Prim.natToNat32 (2 ** 62) == (0 : Nat32));
assert(Prim.natToNat32 (2 ** 62 + 1) == (1 : Nat32));
assert(Prim.natToNat32 (2 ** 62 - 1) == (4294967295 : Nat32));

assert(Prim.natToNat32 (2 ** 63) == (0 : Nat32));
assert(Prim.natToNat32 (2 ** 63 + 1) == (1 : Nat32));
assert(Prim.natToNat32 (2 ** 63 - 1) == (4294967295 : Nat32));

assert(Prim.natToNat32 (2 ** 64) == (0 : Nat32));
assert(Prim.natToNat32 (2 ** 64 + 1) == (1 : Nat32));
assert(Prim.natToNat32 (2 ** 64 - 1) == (4294967295 : Nat32));

