import { explodeNat16; explodeInt16 } = "mo:⛔";
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

//SKIP run
//SKIP run-ir
//SKIP run-low
