// CHECK-LABEL: (func $init

func matchNat(n : Nat) : Bool =
         switch n { case 1073741823 true
                  ; case _ false };
// CHECK-LABEL: (func $matchNat
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i32.const -4
// CHECK-NEXT:   $B_eq

/*

Example derivations:

-1073741824 as binary: 0b11000000000000000000000000000000
(arithmetic) shift left by 1: 0b10000000000000000000000000000000
rotate left by 1: 0b00000000000000000000000000000001
this is 1.

1073741823 as binary: 0b00111111111111111111111111111111
(arithmetic) shift left by 1: 0b01111111111111111111111111111110
rotate left by 1: 0b11111111111111111111111111111100
this is -4.

Note: the topmost 2 bits must be equal for the compact form at all times.
In the encoding result lsb is the sign, the bit left to it must be 0.

*/

func matchInt(n : Int) : Bool =
         switch n { case (-1073741824) true
                  ; case 1073741823 true
                  ; case _ false };
// CHECK-LABEL: (func $matchInt
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i32.const 1
// CHECK-NEXT:   $B_eq
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i32.const -4
// CHECK-NEXT:   $B_eq

func match8(n : Word8) : Bool = switch n { case 42 true; case _ false };
// CHECK-LABEL: (func $match8
// CHECK:        i32.const 704643072
// CHECK-NEXT:   i32.eq
// N.B.: 704643072 == 0x2a000000 == 42 << 24

func match16(n : Word16) : Bool = switch n { case 42 true; case _ false };
// CHECK-LABEL: (func $match16
// CHECK:        i32.const 2752512
// CHECK-NEXT:   i32.eq
// N.B.: 2752512 == 0x002a0000 == 42 << 16

assert (matchNat(1073741823));
assert (matchInt(-1073741824));
assert (matchInt(1073741823));
assert (match8(42));
assert (match16(42));
