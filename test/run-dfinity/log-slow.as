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

for (n in range(0, 127)) {
    for (exp in range(0, 127)) {
        if (n <= 1 or exp <= 1 or (n <= 16 and exp <= 9)) {
            let res = n ** exp;
            if (res <= 127) { assert (intToInt8 n ** intToInt8 exp == intToInt8 res) }
        }
    }
};

assert (intToInt8 2 ** intToInt8 6 == intToInt8 64); // highest exponent
assert (intToInt8 (-2) ** intToInt8 7 == intToInt8 (-128)); // highest exponent
