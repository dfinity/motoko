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


assert(word32ToNat 0 == (0 : Nat));
assert(word32ToNat 42 == (42 : Nat));
assert(word32ToNat 2147483647 == (2147483647 : Nat)); // 2**31 - 1
assert(word32ToNat 4294967295 == (4294967295 : Nat)); // 2**32 - 1


// Int <--> Word32

assert(intToWord32 0 == (0 : Word32));
assert(intToWord32 42 == (42 : Word32));
assert(intToWord32 65535 == (65535 : Word32)); // 2**16 - 1

//printInt (word32ToInt(intToWord32 (-42 : Int)));

assert(intToWord32 (-42 : Int) == (-42/*!*/ : Word32));
assert(intToWord32 (-65535 : Int) == (-65535/*!*/ : Word32)); // - 2**16 + 1
assert(intToWord32 (-65536 : Int) == (-65536/*!*/ : Word32)); // - 2**16

assert(intToWord32 2147483647 == (2147483647 : Word32)); // 2**31 - 1
assert(intToWord32 2147483648 == (2147483648 : Word32)); // 2**31
assert(intToWord32 2147483649 == (2147483649 : Word32)); // 2**31 + 1
assert(intToWord32 4294967295 == (4294967295 : Word32)); // 2**32 - 1


assert(word32ToInt 0 == (0 : Int));
assert(word32ToInt 42 == (42 : Int));
assert(word32ToInt 2147483647 == (2147483647 : Int)); // 2**31 - 1
//assert(word32ToInt 4294967295 == (4294967295 : Int)); // 2**32 - 1

