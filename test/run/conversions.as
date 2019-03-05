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

{
    func roundtrip(n : Nat) = assert (word32ToNat (natToWord32 n) == n);
    roundtrip 0;
    roundtrip 10;
    roundtrip 100;
    roundtrip 1000;
    roundtrip 10000;
    roundtrip 100000;
    roundtrip 1000000;
    roundtrip 10000000;
    roundtrip 100000000;
    roundtrip 1000000000;
};

{
    func roundtrip(w : Word32) = assert (natToWord32 (word32ToNat w) == w);
    roundtrip 0;
    roundtrip 10;
    roundtrip 100;
    roundtrip 1000;
    roundtrip 10000;
    roundtrip 100000;
    roundtrip 1000000;
    roundtrip 10000000;
    roundtrip 100000000;
    roundtrip 1000000000;
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
    roundtrip 0;
    roundtrip 10;
    roundtrip 100;
    roundtrip 1000;
    roundtrip 10000;
    roundtrip 100000;
    roundtrip 1000000;
    roundtrip 10000000;
    roundtrip 100000000;
    roundtrip 1000000000;

    roundtrip (-10);
    roundtrip (-100);
    roundtrip (-1000);
    roundtrip (-10000);
    roundtrip (-100000);
    roundtrip (-1000000);
    roundtrip (-10000000);
    roundtrip (-100000000);
    roundtrip (-1000000000);
};

{
    func roundtrip(w : Word32) = assert (intToWord32 (word32ToInt w) == w);
    roundtrip 0;
    roundtrip 10;
    roundtrip 100;
    roundtrip 1000;
    roundtrip 10000;
    roundtrip 100000;
    roundtrip 1000000;
    roundtrip 10000000;
    roundtrip 100000000;
    roundtrip 1000000000;

    /*!*/
    roundtrip (-10);
    roundtrip (-100);
    roundtrip (-1000);
    roundtrip (-10000);
    roundtrip (-100000);
    roundtrip (-1000000);
    roundtrip (-10000000);
    roundtrip (-100000000);
    roundtrip (-1000000000);
};
