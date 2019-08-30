var i = 0;
for (j in range(0, 10)) {
 printNat(j);
 assert(j == i);
 i += 1;
};
assert(i == 11);
