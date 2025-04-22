import { explodeNat16; explodeInt16; explodeNat32; explodeInt32; explodeNat64; explodeInt64 } = "mo:⛔";

do {
  let (m, l) = explodeNat16 21879;
  assert m == 0x55 and l == 0x77
};

do {
  let (m, l) = explodeInt16 21879;
  assert m == 0x55 and l == 0x77
};

do {
  let (m, l) = explodeInt16 (-21879);
  assert m == 0xaa and l == 0x89
};

do {
  let (m, a, b, l) = explodeNat32 0x55332277;
  assert m == 0x55 and a == 0x33 and b == 0x22 and l == 0x77
};

do {
  let (m, a, b, l) = explodeInt32 0x55332277;
  assert m == 0x55 and a == 0x33 and b == 0x22 and l == 0x77
};

do {
  let (m, a, b, l) = explodeInt32 (-0x55332277);
  assert m == 0xaa and a == 0xcc and b == 0xdd and l == 0x89
};

do {
  let (m, a, b, c, d, e, f, l) = explodeNat64 0x1122334455667788;
  assert m == 0x11 and a == 0x22 and b == 0x33 and c == 0x44 and d == 0x55 and e == 0x66 and f == 0x77 and l == 0x88
};

do {
  let (m, a, b, c, d, e, f, l) = explodeInt64 0x1122334455667788;
  assert m == 0x11 and a == 0x22 and b == 0x33 and c == 0x44 and d == 0x55 and e == 0x66 and f == 0x77 and l == 0x88
};

do {
  let (m, a, b, c, d, e, f, l) = explodeInt64 (-0x1122334455667788);
  assert m == 0xee and a == 0xdd and b == 0xcc and c == 0xbb and d == 0xaa and e == 0x99 and f == 0x88 and l == 0x78
};
