// Word32 operations

func printW32ln(w : Word32) { printInt(word32ToInt w); print "\n" };

let a : Word32 = 4567;
let b : Word32 = 7;
let c : Word32 = 8912765;
let d : Word32 = -15;


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
//printW32ln(sshr d b); // TODO(Gabor)
printW32ln(c <<> b);
printW32ln(c <>> b);
