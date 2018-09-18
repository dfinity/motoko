ignore(3);
ignore(new {});

assert(abs(-3) == 3);

var i = 0;

i := 0;
for (j in range(0, 100)) {
  assert(j == i);
  i += 1;
};
assert(i == 101);

i := 3;
for (j in range(3, 10)) {
  assert(j == i);
  i += 1;
};
assert(i == 11);

for (j in range(3, 3)) {
  assert(j == 3);
};

for (j in range(0, 0)) {
  assert(j == 0);
};

for (j in range(3, 2)) {
  assert(false);
};

i := 30;
for (j in revrange(30, 2)) {
  assert(j == i);
  i -= 1;
};
assert(i == 1);

i := 11;
for (j in revrange(10, 0)) {
  assert(j == i - 1);
  i -= 1;
};
assert(i == 0);

for (j in revrange(3, 3)) {
  assert(j == 3);
};

for (j in revrange(0, 0)) {
  assert(j == 0);
};

for (j in revrange(1, 2)) {
  assert(false);
};
