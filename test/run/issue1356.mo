// CHECK-LABEL: (func $init

func matchNat(n : Nat) : Bool =
         switch n { case 1073741823 true
                  ; case _ false };
// CHECK-LABEL: (func $matchNat
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i64.const 2147483646
// CHECK-NEXT:   call $B_eq

/*

Example derivations:

-1073741824 as binary: 0b11000000000000000000000000000000
(arithmetic) shift left by 1: 0b10000000000000000000000000000000
this is -2147483648.

1073741823 as binary: 0b00111111111111111111111111111111
(arithmetic) shift left by 1: 0b01111111111111111111111111111110
this is 2147483646.

*/

func matchInt(n : Int) : Bool =
         switch n { case (-1073741824) true
                  ; case 1073741823 true
                  ; case _ false };
// CHECK-LABEL: (func $matchInt
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i64.const -2147483648
// CHECK-NEXT:   $B_eq
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i64.const 2147483646
// CHECK-NEXT:   $B_eq

func match8(n : Nat8) : Bool = switch n { case 42 true; case _ false };
// CHECK-LABEL: (func $match8
// CHECK:        i64.const 3026418949592973312
// CHECK-NEXT:   i64.eq
// N.B.: 3026418949592973312 == 0x2a00_0000_0000_0000 == 42 << 56

func match16(n : Nat16) : Bool = switch n { case 42 true; case _ false };
// CHECK-LABEL: (func $match16
// CHECK:        i64.const 11821949021847552
// CHECK-NEXT:   i64.eq
// N.B.: 11821949021847552 == 0x002a_0000_0000_0000 == 42 << 48

// NB: reverse order, so that things appear in order
assert (match16(42));
assert (match8(42));
assert (matchInt(1073741823));
assert (matchInt(-1073741824));
assert (matchNat(1073741823));
