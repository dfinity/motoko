//CLASSICAL-PERSISTENCE-ONLY
// FHECK-LABEL: (func $init

func matchNat(n : Nat) : Bool =
         switch n { case 1073741823 true
                  ; case _ false };
// FHECK-LABEL: (func $matchNat
// FHECK:        local.get $switch_in
// FHECK-NEXT:   i32.const 2147483646
// FHECK-NEXT:   call $B_eq

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
// FHECK-LABEL: (func $matchInt
// FHECK:        local.get $switch_in
// FHECK-NEXT:   i32.const -2147483648
// FHECK-NEXT:   $B_eq
// FHECK:        local.get $switch_in
// FHECK-NEXT:   i32.const 2147483646
// FHECK-NEXT:   $B_eq

func match8(n : Nat8) : Bool = switch n { case 42 true; case _ false };
// FHECK-LABEL: (func $match8
// FHECK:        i32.const 704643072
// FHECK-NEXT:   i32.ne
// N.B.: 704643072 == 0x2a000000 == 42 << 24

func match16(n : Nat16) : Bool = switch n { case 42 true; case _ false };
// FHECK-LABEL: (func $match16
// FHECK:        i32.const 2752512
// FHECK-NEXT:   i32.ne
// N.B.: 2752512 == 0x002a0000 == 42 << 16

// NB: reverse order, so that things appear in order
assert (match16(42));
assert (match8(42));
assert (matchInt(1073741823));
assert (matchInt(-1073741824));
assert (matchNat(1073741823));
