import Prim "mo:⛔";

// `shiftLeft` must agree with `n * 2 ** amount` across the compact range limit
// (2^61), including where the inline fast path for small operands applies.

func check(n : Nat, amount : Nat32) {
  let expected = n * 2 ** Prim.nat32ToNat amount;
  if (Prim.shiftLeft(n, amount) != expected) {
    Prim.debugPrint(debug_show (n, amount));
    assert false
  }
};

check(2 ** 30, 31);           // 2^61, the first value outside the compact range
check(2 ** 30, 32);           // 2^62
check(2 ** 29, 32);           // 2^61
check(4_294_967_295, 32);     // the largest product with both operands small

var k = 0;
while (k <= 33) {
  for (n in [Prim.abs(2 ** k - 1 : Int), 2 ** k, 2 ** k + 1].values()) {
    var a : Nat32 = 0;
    while (a < 64) { check(n, a); a += 1 }
  };
  k += 1
};
