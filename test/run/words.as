// CHECK: func $start
// Word32 operations
{
    func printW32ln(w : Word32) { printInt(word32ToNat w);  print " "; printInt(word32ToInt w); print "\n" };

    let a : Word32 = 4567;
    let b : Word32 = 7;
    let c : Word32 = 8912765;
    let d : Word32 = -15;
    let e : Word32 = 20000;

// CHECK: get_local $c
// LATER: HECK-NOT: call $box_i32
// CHECK: call $printW32ln
    printW32ln(+c);
// CHECK: call $printW32ln
    printW32ln(-c);
// CHECK: call $printW32ln
    printW32ln(^c);
// CHECK: call $printW32ln
    printW32ln(a + c);
// CHECK: call $printW32ln
    printW32ln(c - a);

// CHECK-NOT: i32.shr_u
// CHECK: call $printW32ln
    printW32ln(a * b);
// CHECK: call $printW32ln
    printW32ln(a / b);
// CHECK: call $printW32ln
    printW32ln(c % a);
// CHECK: call $printW32ln
    printW32ln(a ** 2);

// CHECK: call $printW32ln
    printW32ln(a & c);
// CHECK: call $printW32ln
    printW32ln(a | c);
// CHECK: call $printW32ln
    printW32ln(a ^ c);
// CHECK: call $printW32ln
    printW32ln(a << b);
// CHECK: call $printW32ln
    printW32ln(a >> b);
    // printW32ln(shrs d b); // TODO(Gabor)
// CHECK: call $printW32ln
    printW32ln(c <<> b);
// CHECK: call $printW32ln
    printW32ln(c <>> b);
    // printW32ln(popcnt d); // TODO(Gabor)
    // printW32ln(clz c); // TODO(Gabor)
    // printW32ln(ctz e); // TODO(Gabor)
};

// Word16 operations
{
    func printW16ln(w : Word16) { printInt(word16ToNat w); print " "; printInt(word16ToInt w); print "\n" };

    let a : Word16 = 4567;
    let b : Word16 = 7;
    let c : Word16 = 55734;
    let d : Word16 = -15;
    let e : Word16 = 20000;


// CHECK: call $printW16ln
    printW16ln(+c);
// CHECK: call $printW16ln
    printW16ln(-c);
// CHECK: call $printW16ln
    printW16ln(^c);
// CHECK: call $printW16ln
    printW16ln(a + c);
// CHECK: call $printW16ln
    printW16ln(c - a);

// CHECK: get_local $a
// CHECK: get_local $b
// CHECK: i32.const 16
// CHECK: i32.shr_u
// CHECK: i32.mul
// CHECK: call $printW16ln
    printW16ln(a * b);
// CHECK: call $printW16ln
    printW16ln(a / b);
// CHECK: call $printW16ln
    printW16ln(c % a);
// CHECK: call $printW16ln
    printW16ln(a ** 2);

// CHECK: call $printW16ln
    printW16ln(a & c);
// CHECK: call $printW16ln
    printW16ln(a | c);
// CHECK: call $printW16ln
    printW16ln(a ^ c);
// CHECK: call $printW16ln
    printW16ln(a << b);

// CHECK: get_local $b
// CHECK-NEXT: i32.const 15
// CHECK-NEXT: i32.and
// CHECK-NEXT: i32.shr_u
// CHECK-NEXT: i32.const -65536
// CHECK-NEXT: i32.and
// CHECK-NEXT: call $printW16ln
    printW16ln(a >> b);
    // printW16ln(shrs d b); // TODO(Gabor)

// CHECK: call $printW16ln
    printW16ln(c <<> b);

// CHECK: get_local $b
// CHECK-NEXT: call $rotr<Word16>
// CHECK-NEXT: call $printW16ln
    printW16ln(c <>> b);
    // printW16ln(popcnt d); // TODO(Gabor)
    // printW16ln(clz c); // TODO(Gabor)
    // printW16ln(ctz e); // TODO(Gabor)
};

// Word8 operations
{
    func printW8ln(w : Word8) { printInt(word8ToNat w); print " "; printInt(word8ToInt w); print "\n" };

    let a : Word8 = 67;
    let b : Word8 = 7;
    let c : Word8 = 34;
    let d : Word8 = -15;
    let e : Word8 = 200;


// CHECK: call $printW8ln
    printW8ln(+c);
// CHECK: call $printW8ln
    printW8ln(-c);
// CHECK: call $printW8ln
    printW8ln(^c);
// CHECK: call $printW8ln
    printW8ln(a + c);
// CHECK: call $printW8ln
    printW8ln(c - a);
// CHECK: call $printW8ln
    printW8ln(a * b);
// CHECK: call $printW8ln
    printW8ln(a / b);
// CHECK: call $printW8ln
    printW8ln(c % a);
// CHECK: call $printW8ln
    printW8ln(a ** 2);

// CHECK: call $printW8ln
    printW8ln(a & c);
// CHECK: call $printW8ln
    printW8ln(a | c);
// CHECK: call $printW8ln
    printW8ln(a ^ c);
// CHECK: call $printW8ln
    printW8ln(a << b);

// CHECK: get_local $b
// CHECK-NEXT: i32.const 7
// CHECK-NEXT: i32.and
// CHECK-NEXT: i32.shr_u
// CHECK-NEXT: i32.const -16777216
// CHECK-NEXT: i32.and
// CHECK-NEXT: call $printW8ln
    printW8ln(a >> b);
    // printW8ln(shrs d b); // TODO(Gabor)

// CHECK: call $printW8ln
    printW8ln(c <<> b);
// CHECK: call $printW8ln
    printW8ln(c <>> b);
    // printW8ln(popcnt d); // TODO(Gabor)
    // printW8ln(clz c); // TODO(Gabor)
    // printW8ln(ctz e); // TODO(Gabor)
};
