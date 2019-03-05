// Word32 operations
{
    func printW32ln(w : Word32) { printInt(word32ToNat w);  print " "; printInt(word32ToInt w); print "\n" };

    let a : Word32 = 4567;
    let b : Word32 = 7;
    let c : Word32 = 8912765;
    let d : Word32 = -15;
    let e : Word32 = 20000;


    printW32ln(+c);
    printW32ln(-c);
    printW32ln(^c);
    printW32ln(a + c);
    printW32ln(c - a);
    printW32ln(a * b);
    printW32ln(a / b);
    printW32ln(c % a);
    printW32ln(a ** 2);

    printW32ln(a & c);
    printW32ln(a | c);
    printW32ln(a ^ c);
    printW32ln(a << b);
    printW32ln(a >> b);
    // printW32ln(shrs d b); // TODO(Gabor)
    printW32ln(c <<> b);
    printW32ln(c <>> b);
    // printW32ln(lognot d); // TODO(Gabor)
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


    printW16ln(+c);
    printW16ln(-c);
    printW16ln(^c);
    printW16ln(a + c);
    printW16ln(c - a);
    printW16ln(a * b);
    printW16ln(a / b);
    printW16ln(c % a);
    printW16ln(a ** 2);

    printW16ln(a & c);
    printW16ln(a | c);
    printW16ln(a ^ c);
    printW16ln(a << b);
    printW16ln(a >> b);
    // printW16ln(shrs d b); // TODO(Gabor)
    printW16ln(c <<> b);
    printW16ln(c <>> b);
    // printW16ln(lognot d); // TODO(Gabor)
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


    printW8ln(+c);
    printW8ln(-c);
    printW8ln(^c);
    printW8ln(a + c);
    printW8ln(c - a);
    printW8ln(a * b);
    printW8ln(a / b);
    printW8ln(c % a);
    printW8ln(a ** 2);

    printW8ln(a & c);
    printW8ln(a | c);
    printW8ln(a ^ c);
    printW8ln(a << b);
    printW8ln(a >> b);
    // printW8ln(shrs d b); // TODO(Gabor)
    printW8ln(c <<> b);
    printW8ln(c <>> b);
    // printW8ln(lognot d); // TODO(Gabor)
    // printW8ln(clz c); // TODO(Gabor)
    // printW8ln(ctz e); // TODO(Gabor)
};
