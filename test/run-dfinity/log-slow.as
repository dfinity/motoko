assert (natToNat32 2 ** natToNat32 31 == natToNat32 2_147_483_648); // highest exponent




// Nat*

for (n in range(0, 255)) {
    for (exp in range(0, 255)) {
        if (n <= 1 or exp <= 1 or (n <= 16 and exp <= 9)) {
            let res = n ** exp;
            if (res <= 255) { assert (natToNat8 n ** natToNat8 exp == natToNat8 res) }
        }
    }
};

assert (natToNat8 2 ** natToNat8 7 == natToNat8 128); // highest exponent

for (n in range(0, 255)) {
    for (exp in range(0, 255)) {
        if (n <= 1 or exp <= 1 or (n <= 16 and exp <= 9)) { // see #537
            let res = n ** exp;
            if (res <= 255) { assert (natToNat8 n ** natToNat8 exp == natToNat8 res) }
        }
    }
};


assert (natToNat16 2 ** natToNat16 15 == natToNat16 32768); // highest exponent

for (n in range(0, 255)) {
    for (exp in range(0, 255)) {
        if (n <= 1 or exp <= 106) { // see #537
            let res = n ** exp;
            if (res <= 65535) { assert (natToNat16 n ** natToNat16 exp == natToNat16 res) }
        }
    }
};


assert (natToNat32 2 ** natToNat32 31 == natToNat32 2_147_483_648); // highest exponent

for (n in range(0, 255)) {
    for (exp in range(0, 255)) {
        if (n <= 1 or exp <= 106) { // see #537
            let res = n ** exp;
            if (res <= 65535) { assert (natToNat32 n ** natToNat32 exp == natToNat32 res) }
        }
    }
};


// Int*

assert (intToInt8 2 ** intToInt8 6 == intToInt8 64); // highest exponent

for (n in range(0, 127)) {
    for (exp in range(0, 127)) {
        if (n <= 1 or exp <= 1 or (n <= 16 and exp <= 9)) {
            let res = n ** exp;
            if (res <= 127) { assert (intToInt8 n ** intToInt8 exp == intToInt8 res) }
        }
    }
};


assert (intToInt8 (-2) ** intToInt8 7 == intToInt8 (-128)); // highest exponent

{
var n = -128;
while (n < -1) {
    for (exp in range(0, 127)) {
        if (n == -1 or exp <= 1 or (n >= -17 and exp <= 9)) {
            let res = n ** exp;
            if (res >= -128 and res <= 127) {
                assert (intToInt8 n ** intToInt8 exp == intToInt8 res)
            }
        }
    };
    n += 1
}
};



assert (intToInt16 2 ** intToInt16 14 == intToInt16 16384); // highest exponent

for (n in range(0, 127)) {
    for (exp in range(0, 127)) {
        if (n <= 1 or exp <= 1 or (n <= 256 and exp <= 14)) {
            let res = n ** exp;
            if (res <= 32767) { assert (intToInt16 n ** intToInt16 exp == intToInt16 res) }
        }
    }
};


assert (intToInt16 (-2) ** intToInt16 15 == intToInt16 (-32768)); // highest exponent

{
var n = -128;
while (n < -1) {
    for (exp in range(0, 127)) {
        if (n == -1 or exp <= 1 or (n >= -257 and exp <= 15)) {
            let res = n ** exp;
            if (res >= -32768 and res <= 32767) {
                assert (intToInt16 n ** intToInt16 exp == intToInt16 res)
            }
        }
    };
    n += 1
}
};


assert (intToInt32 (-2) ** intToInt32 31 == intToInt32 (-2_147_483_648)); // highest exponent
