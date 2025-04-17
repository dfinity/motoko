import { explodeNat16; explodeInt16; explodeNat32; explodeInt32 } = "mo:⛔";
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


//SKIP run
//SKIP run-ir
//SKIP run-low
