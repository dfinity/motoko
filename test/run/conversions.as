func n2w(n : Nat) : ?Word32 { ?(natToWord32 n) };

assert(natToWord32 42 == (42 : Word32));
assert(natToWord32 65535 == (65535 : Word32)); // 2**16 - 1

assert(natToWord32 2147483647 == (2147483647 : Word32)); // 2**31 - 1
// assert(natToWord32 2147483648 == (2147483648 : Word32)); // 2**31
// assert(natToWord32 4294967295 == (4294967295 : Word32)); TODO: 31 bits currently
