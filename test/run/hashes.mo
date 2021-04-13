import Prim "mo:⛔";

func hashInt(x : Int) : Word32 {
  var n = x;
  var hash : Word32 = 0;
  if (n < 0) {
    hash := ^hash;
    n := Prim.abs n;
  };
  let base = 2**32;
  while (n > 0) {
    hash ^= Prim.intToWord32Wrap(n % base);
    n /= base;
  };
  return hash;
};


assert (hashInt (10**7) == (10000000 : Word32));
assert (hashInt 0 == (0 : Word32));
assert (hashInt (10**18) == (2_860_824_243 : Word32));

assert (hashInt (-1) == (-2 : Word32));
assert (hashInt (-387) == (-388 : Word32));
assert (hashInt (-3876548352991) == (1807116198 : Word32));
