func matchNat(n : Nat) : Bool =
         switch n { case 1073741823 true
                  ; case _ false };
// CHECK-LABEL: (func $matchNat
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i32.const -4

func matchInt(n : Int) : Bool =
         switch n { case (-1073741824) true
                  ; case 1073741823 true
                  ; case _ false };
// CHECK-LABEL: (func $matchInt
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i32.const 1
// CHECK:        local.get $switch_in
// CHECK-NEXT:   i32.const -4

func match8(n : Word8) : Bool = switch n { case 42 true; case _ false };
// CHECK-LABEL: (func $match8
// CHECK:        i32.const 704643072

func match16(n : Word16) : Bool = switch n { case 42 true; case _ false };
// CHECK-LABEL: (func $match16
// CHECK:        i32.const 2752512

assert (matchNat(1073741823));
assert (matchInt(-1073741824));
assert (matchInt(1073741823));
assert (match8(42));
assert (match16(42));

// CHECK-LABEL: (func $start
