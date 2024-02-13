import Blob "./sha224/Blob";
import Array "./sha224/Array";
import Nat8 "./sha224/Nat8";
import Nat32 "./sha224/Nat32";
import Nat64 "./sha224/Nat64";
import { Array_tabulate; performanceCounter; debugPrint; rts_heap_size } = "mo:⛔";

actor {

  func counters() : (Int, Nat64) = (rts_heap_size(), performanceCounter(0));

  public func go() : async () {
    let (m0, n0) = counters();
    let sha224 = SHA224();
    let data = Array.tabulate(1024*1024, Nat8.fromIntWrap);
    sha224.writeArray(data);
    let hashSum = sha224.sum();
    let (m1, n1) = counters();
    debugPrint(debug_show (m1 - m0, n1 - n0));
  };


  /**
  * SHA224 Utilities used in toAccount().
  * Utilities are not exposed as public functions.
  * Taken with permission from https://github.com/research-ag/sha2
  **/
  let K00 : Nat32 = 0x428a2f98;
  let K01 : Nat32 = 0x71374491;
  let K02 : Nat32 = 0xb5c0fbcf;
  let K03 : Nat32 = 0xe9b5dba5;
  let K04 : Nat32 = 0x3956c25b;
  let K05 : Nat32 = 0x59f111f1;
  let K06 : Nat32 = 0x923f82a4;
  let K07 : Nat32 = 0xab1c5ed5;
  let K08 : Nat32 = 0xd807aa98;
  let K09 : Nat32 = 0x12835b01;
  let K10 : Nat32 = 0x243185be;
  let K11 : Nat32 = 0x550c7dc3;
  let K12 : Nat32 = 0x72be5d74;
  let K13 : Nat32 = 0x80deb1fe;
  let K14 : Nat32 = 0x9bdc06a7;
  let K15 : Nat32 = 0xc19bf174;
  let K16 : Nat32 = 0xe49b69c1;
  let K17 : Nat32 = 0xefbe4786;
  let K18 : Nat32 = 0x0fc19dc6;
  let K19 : Nat32 = 0x240ca1cc;
  let K20 : Nat32 = 0x2de92c6f;
  let K21 : Nat32 = 0x4a7484aa;
  let K22 : Nat32 = 0x5cb0a9dc;
  let K23 : Nat32 = 0x76f988da;
  let K24 : Nat32 = 0x983e5152;
  let K25 : Nat32 = 0xa831c66d;
  let K26 : Nat32 = 0xb00327c8;
  let K27 : Nat32 = 0xbf597fc7;
  let K28 : Nat32 = 0xc6e00bf3;
  let K29 : Nat32 = 0xd5a79147;
  let K30 : Nat32 = 0x06ca6351;
  let K31 : Nat32 = 0x14292967;
  let K32 : Nat32 = 0x27b70a85;
  let K33 : Nat32 = 0x2e1b2138;
  let K34 : Nat32 = 0x4d2c6dfc;
  let K35 : Nat32 = 0x53380d13;
  let K36 : Nat32 = 0x650a7354;
  let K37 : Nat32 = 0x766a0abb;
  let K38 : Nat32 = 0x81c2c92e;
  let K39 : Nat32 = 0x92722c85;
  let K40 : Nat32 = 0xa2bfe8a1;
  let K41 : Nat32 = 0xa81a664b;
  let K42 : Nat32 = 0xc24b8b70;
  let K43 : Nat32 = 0xc76c51a3;
  let K44 : Nat32 = 0xd192e819;
  let K45 : Nat32 = 0xd6990624;
  let K46 : Nat32 = 0xf40e3585;
  let K47 : Nat32 = 0x106aa070;
  let K48 : Nat32 = 0x19a4c116;
  let K49 : Nat32 = 0x1e376c08;
  let K50 : Nat32 = 0x2748774c;
  let K51 : Nat32 = 0x34b0bcb5;
  let K52 : Nat32 = 0x391c0cb3;
  let K53 : Nat32 = 0x4ed8aa4a;
  let K54 : Nat32 = 0x5b9cca4f;
  let K55 : Nat32 = 0x682e6ff3;
  let K56 : Nat32 = 0x748f82ee;
  let K57 : Nat32 = 0x78a5636f;
  let K58 : Nat32 = 0x84c87814;
  let K59 : Nat32 = 0x8cc70208;
  let K60 : Nat32 = 0x90befffa;
  let K61 : Nat32 = 0xa4506ceb;
  let K62 : Nat32 = 0xbef9a3f7;
  let K63 : Nat32 = 0xc67178f2;

  let ivs : [[Nat32]] = [
    [
      // 224
      0xc1059ed8,
      0x367cd507,
      0x3070dd17,
      0xf70e5939,
      0xffc00b31,
      0x68581511,
      0x64f98fa7,
      0xbefa4fa4
    ],
    [
      // 256
      0x6a09e667,
      0xbb67ae85,
      0x3c6ef372,
      0xa54ff53a,
      0x510e527f,
      0x9b05688c,
      0x1f83d9ab,
      0x5be0cd19
    ]
  ];

  let rot = Nat32.bitrotRight;

  class SHA224() {
    let (sum_bytes, iv) = (28, 0);

    var s0 : Nat32 = 0;
    var s1 : Nat32 = 0;
    var s2 : Nat32 = 0;
    var s3 : Nat32 = 0;
    var s4 : Nat32 = 0;
    var s5 : Nat32 = 0;
    var s6 : Nat32 = 0;
    var s7 : Nat32 = 0;

    let msg : [var Nat32] = Array.init<Nat32>(16, 0);
    let digest = Array.init<Nat8>(sum_bytes, 0);
    var word : Nat32 = 0;

    var i_msg : Nat8 = 0;
    var i_byte : Nat8 = 4;
    var i_block : Nat64 = 0;

    public func reset() {
      i_msg := 0;
      i_byte := 4;
      i_block := 0;
      s0 := ivs[iv][0];
      s1 := ivs[iv][1];
      s2 := ivs[iv][2];
      s3 := ivs[iv][3];
      s4 := ivs[iv][4];
      s5 := ivs[iv][5];
      s6 := ivs[iv][6];
      s7 := ivs[iv][7]
    };

    reset();

    private func writeByte(val : Nat8) : () {
      word := (word << 8) ^ Nat32.fromIntWrap(Nat8.toNat(val));
      i_byte -%= 1;
      if (i_byte == 0) {
        msg[Nat8.toNat(i_msg)] := word;
        word := 0;
        i_byte := 4;
        i_msg +%= 1;
        if (i_msg == 16) {
          process_block();
          i_msg := 0;
          i_block +%= 1
        }
      }
    };

    private func process_block() : () {
      let w00 = msg[0];
      let w01 = msg[1];
      let w02 = msg[2];
      let w03 = msg[3];
      let w04 = msg[4];
      let w05 = msg[5];
      let w06 = msg[6];
      let w07 = msg[7];
      let w08 = msg[8];
      let w09 = msg[9];
      let w10 = msg[10];
      let w11 = msg[11];
      let w12 = msg[12];
      let w13 = msg[13];
      let w14 = msg[14];
      let w15 = msg[15];
      let w16 = w00 +% rot(w01, 07) ^ rot(w01, 18) ^ (w01 >> 03) +% w09 +% rot(w14, 17) ^ rot(w14, 19) ^ (w14 >> 10);
      let w17 = w01 +% rot(w02, 07) ^ rot(w02, 18) ^ (w02 >> 03) +% w10 +% rot(w15, 17) ^ rot(w15, 19) ^ (w15 >> 10);
      let w18 = w02 +% rot(w03, 07) ^ rot(w03, 18) ^ (w03 >> 03) +% w11 +% rot(w16, 17) ^ rot(w16, 19) ^ (w16 >> 10);
      let w19 = w03 +% rot(w04, 07) ^ rot(w04, 18) ^ (w04 >> 03) +% w12 +% rot(w17, 17) ^ rot(w17, 19) ^ (w17 >> 10);
      let w20 = w04 +% rot(w05, 07) ^ rot(w05, 18) ^ (w05 >> 03) +% w13 +% rot(w18, 17) ^ rot(w18, 19) ^ (w18 >> 10);
      let w21 = w05 +% rot(w06, 07) ^ rot(w06, 18) ^ (w06 >> 03) +% w14 +% rot(w19, 17) ^ rot(w19, 19) ^ (w19 >> 10);
      let w22 = w06 +% rot(w07, 07) ^ rot(w07, 18) ^ (w07 >> 03) +% w15 +% rot(w20, 17) ^ rot(w20, 19) ^ (w20 >> 10);
      let w23 = w07 +% rot(w08, 07) ^ rot(w08, 18) ^ (w08 >> 03) +% w16 +% rot(w21, 17) ^ rot(w21, 19) ^ (w21 >> 10);
      let w24 = w08 +% rot(w09, 07) ^ rot(w09, 18) ^ (w09 >> 03) +% w17 +% rot(w22, 17) ^ rot(w22, 19) ^ (w22 >> 10);
      let w25 = w09 +% rot(w10, 07) ^ rot(w10, 18) ^ (w10 >> 03) +% w18 +% rot(w23, 17) ^ rot(w23, 19) ^ (w23 >> 10);
      let w26 = w10 +% rot(w11, 07) ^ rot(w11, 18) ^ (w11 >> 03) +% w19 +% rot(w24, 17) ^ rot(w24, 19) ^ (w24 >> 10);
      let w27 = w11 +% rot(w12, 07) ^ rot(w12, 18) ^ (w12 >> 03) +% w20 +% rot(w25, 17) ^ rot(w25, 19) ^ (w25 >> 10);
      let w28 = w12 +% rot(w13, 07) ^ rot(w13, 18) ^ (w13 >> 03) +% w21 +% rot(w26, 17) ^ rot(w26, 19) ^ (w26 >> 10);
      let w29 = w13 +% rot(w14, 07) ^ rot(w14, 18) ^ (w14 >> 03) +% w22 +% rot(w27, 17) ^ rot(w27, 19) ^ (w27 >> 10);
      let w30 = w14 +% rot(w15, 07) ^ rot(w15, 18) ^ (w15 >> 03) +% w23 +% rot(w28, 17) ^ rot(w28, 19) ^ (w28 >> 10);
      let w31 = w15 +% rot(w16, 07) ^ rot(w16, 18) ^ (w16 >> 03) +% w24 +% rot(w29, 17) ^ rot(w29, 19) ^ (w29 >> 10);
      let w32 = w16 +% rot(w17, 07) ^ rot(w17, 18) ^ (w17 >> 03) +% w25 +% rot(w30, 17) ^ rot(w30, 19) ^ (w30 >> 10);
      let w33 = w17 +% rot(w18, 07) ^ rot(w18, 18) ^ (w18 >> 03) +% w26 +% rot(w31, 17) ^ rot(w31, 19) ^ (w31 >> 10);
      let w34 = w18 +% rot(w19, 07) ^ rot(w19, 18) ^ (w19 >> 03) +% w27 +% rot(w32, 17) ^ rot(w32, 19) ^ (w32 >> 10);
      let w35 = w19 +% rot(w20, 07) ^ rot(w20, 18) ^ (w20 >> 03) +% w28 +% rot(w33, 17) ^ rot(w33, 19) ^ (w33 >> 10);
      let w36 = w20 +% rot(w21, 07) ^ rot(w21, 18) ^ (w21 >> 03) +% w29 +% rot(w34, 17) ^ rot(w34, 19) ^ (w34 >> 10);
      let w37 = w21 +% rot(w22, 07) ^ rot(w22, 18) ^ (w22 >> 03) +% w30 +% rot(w35, 17) ^ rot(w35, 19) ^ (w35 >> 10);
      let w38 = w22 +% rot(w23, 07) ^ rot(w23, 18) ^ (w23 >> 03) +% w31 +% rot(w36, 17) ^ rot(w36, 19) ^ (w36 >> 10);
      let w39 = w23 +% rot(w24, 07) ^ rot(w24, 18) ^ (w24 >> 03) +% w32 +% rot(w37, 17) ^ rot(w37, 19) ^ (w37 >> 10);
      let w40 = w24 +% rot(w25, 07) ^ rot(w25, 18) ^ (w25 >> 03) +% w33 +% rot(w38, 17) ^ rot(w38, 19) ^ (w38 >> 10);
      let w41 = w25 +% rot(w26, 07) ^ rot(w26, 18) ^ (w26 >> 03) +% w34 +% rot(w39, 17) ^ rot(w39, 19) ^ (w39 >> 10);
      let w42 = w26 +% rot(w27, 07) ^ rot(w27, 18) ^ (w27 >> 03) +% w35 +% rot(w40, 17) ^ rot(w40, 19) ^ (w40 >> 10);
      let w43 = w27 +% rot(w28, 07) ^ rot(w28, 18) ^ (w28 >> 03) +% w36 +% rot(w41, 17) ^ rot(w41, 19) ^ (w41 >> 10);
      let w44 = w28 +% rot(w29, 07) ^ rot(w29, 18) ^ (w29 >> 03) +% w37 +% rot(w42, 17) ^ rot(w42, 19) ^ (w42 >> 10);
      let w45 = w29 +% rot(w30, 07) ^ rot(w30, 18) ^ (w30 >> 03) +% w38 +% rot(w43, 17) ^ rot(w43, 19) ^ (w43 >> 10);
      let w46 = w30 +% rot(w31, 07) ^ rot(w31, 18) ^ (w31 >> 03) +% w39 +% rot(w44, 17) ^ rot(w44, 19) ^ (w44 >> 10);
      let w47 = w31 +% rot(w32, 07) ^ rot(w32, 18) ^ (w32 >> 03) +% w40 +% rot(w45, 17) ^ rot(w45, 19) ^ (w45 >> 10);
      let w48 = w32 +% rot(w33, 07) ^ rot(w33, 18) ^ (w33 >> 03) +% w41 +% rot(w46, 17) ^ rot(w46, 19) ^ (w46 >> 10);
      let w49 = w33 +% rot(w34, 07) ^ rot(w34, 18) ^ (w34 >> 03) +% w42 +% rot(w47, 17) ^ rot(w47, 19) ^ (w47 >> 10);
      let w50 = w34 +% rot(w35, 07) ^ rot(w35, 18) ^ (w35 >> 03) +% w43 +% rot(w48, 17) ^ rot(w48, 19) ^ (w48 >> 10);
      let w51 = w35 +% rot(w36, 07) ^ rot(w36, 18) ^ (w36 >> 03) +% w44 +% rot(w49, 17) ^ rot(w49, 19) ^ (w49 >> 10);
      let w52 = w36 +% rot(w37, 07) ^ rot(w37, 18) ^ (w37 >> 03) +% w45 +% rot(w50, 17) ^ rot(w50, 19) ^ (w50 >> 10);
      let w53 = w37 +% rot(w38, 07) ^ rot(w38, 18) ^ (w38 >> 03) +% w46 +% rot(w51, 17) ^ rot(w51, 19) ^ (w51 >> 10);
      let w54 = w38 +% rot(w39, 07) ^ rot(w39, 18) ^ (w39 >> 03) +% w47 +% rot(w52, 17) ^ rot(w52, 19) ^ (w52 >> 10);
      let w55 = w39 +% rot(w40, 07) ^ rot(w40, 18) ^ (w40 >> 03) +% w48 +% rot(w53, 17) ^ rot(w53, 19) ^ (w53 >> 10);
      let w56 = w40 +% rot(w41, 07) ^ rot(w41, 18) ^ (w41 >> 03) +% w49 +% rot(w54, 17) ^ rot(w54, 19) ^ (w54 >> 10);
      let w57 = w41 +% rot(w42, 07) ^ rot(w42, 18) ^ (w42 >> 03) +% w50 +% rot(w55, 17) ^ rot(w55, 19) ^ (w55 >> 10);
      let w58 = w42 +% rot(w43, 07) ^ rot(w43, 18) ^ (w43 >> 03) +% w51 +% rot(w56, 17) ^ rot(w56, 19) ^ (w56 >> 10);
      let w59 = w43 +% rot(w44, 07) ^ rot(w44, 18) ^ (w44 >> 03) +% w52 +% rot(w57, 17) ^ rot(w57, 19) ^ (w57 >> 10);
      let w60 = w44 +% rot(w45, 07) ^ rot(w45, 18) ^ (w45 >> 03) +% w53 +% rot(w58, 17) ^ rot(w58, 19) ^ (w58 >> 10);
      let w61 = w45 +% rot(w46, 07) ^ rot(w46, 18) ^ (w46 >> 03) +% w54 +% rot(w59, 17) ^ rot(w59, 19) ^ (w59 >> 10);
      let w62 = w46 +% rot(w47, 07) ^ rot(w47, 18) ^ (w47 >> 03) +% w55 +% rot(w60, 17) ^ rot(w60, 19) ^ (w60 >> 10);
      let w63 = w47 +% rot(w48, 07) ^ rot(w48, 18) ^ (w48 >> 03) +% w56 +% rot(w61, 17) ^ rot(w61, 19) ^ (w61 >> 10);

      /*
      for ((i, j, k, l, m) in expansion_rounds.vals()) {
        // (j,k,l,m) = (i+1,i+9,i+14,i+16)
        let (v0, v1) = (msg[j], msg[l]);
        let s0 = rot(v0, 07) ^ rot(v0, 18) ^ (v0 >> 03);
        let s1 = rot(v1, 17) ^ rot(v1, 19) ^ (v1 >> 10);
        msg[m] := msg[i] +% s0 +% msg[k] +% s1;
      };
      */
      // compress
      var a = s0;
      var b = s1;
      var c = s2;
      var d = s3;
      var e = s4;
      var f = s5;
      var g = s6;
      var h = s7;
      var t = 0 : Nat32;

      t := h +% K00 +% w00 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K01 +% w01 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K02 +% w02 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K03 +% w03 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K04 +% w04 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K05 +% w05 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K06 +% w06 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K07 +% w07 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K08 +% w08 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K09 +% w09 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K10 +% w10 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K11 +% w11 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K12 +% w12 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K13 +% w13 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K14 +% w14 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K15 +% w15 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K16 +% w16 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K17 +% w17 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K18 +% w18 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K19 +% w19 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K20 +% w20 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K21 +% w21 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K22 +% w22 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K23 +% w23 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K24 +% w24 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K25 +% w25 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K26 +% w26 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K27 +% w27 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K28 +% w28 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K29 +% w29 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K30 +% w30 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K31 +% w31 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K32 +% w32 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K33 +% w33 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K34 +% w34 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K35 +% w35 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K36 +% w36 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K37 +% w37 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K38 +% w38 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K39 +% w39 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K40 +% w40 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K41 +% w41 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K42 +% w42 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K43 +% w43 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K44 +% w44 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K45 +% w45 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K46 +% w46 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K47 +% w47 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K48 +% w48 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K49 +% w49 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K50 +% w50 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K51 +% w51 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K52 +% w52 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K53 +% w53 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K54 +% w54 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K55 +% w55 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K56 +% w56 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K57 +% w57 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K58 +% w58 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K59 +% w59 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K60 +% w60 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K61 +% w61 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K62 +% w62 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
      t := h +% K63 +% w63 +% (e & f) ^ (^ e & g) +% rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
      h := g;
      g := f;
      f := e;
      e := d +% t;
      d := c;
      c := b;
      b := a;
      a := t +% (b & c) ^ (b & d) ^ (c & d) +% rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);

      /*
      for (i in compression_rounds.keys()) {
        let ch = (e & f) ^ (^ e & g);
        let maj = (a & b) ^ (a & c) ^ (b & c);
        let sigma0 = rot(a, 02) ^ rot(a, 13) ^ rot(a, 22);
        let sigma1 = rot(e, 06) ^ rot(e, 11) ^ rot(e, 25);
        let t = h +% K[i] +% msg[i] +% ch +% sigma1;
        h := g;
        g := f;
        f := e;
        e := d +% t;
        d := c;
        c := b;
        b := a;
        a := t +% maj +% sigma0;
      };
      */
      // final addition
      s0 +%= a;
      s1 +%= b;
      s2 +%= c;
      s3 +%= d;
      s4 +%= e;
      s5 +%= f;
      s6 +%= g;
      s7 +%= h
    };

    public func writeIter(iter : { next() : ?Nat8 }) : () {
      label reading loop {
        switch (iter.next()) {
          case (?val) {
            writeByte(val);
            continue reading
          };
          case (null) {
            break reading
          }
        }
      }
    };

    public func writeArray(arr : [Nat8]) : () = writeIter(arr.vals());
    public func writeBlob(blob : Blob) : () = writeIter(blob.vals());

    public func sum() : Blob {
      // calculate padding
      // t = bytes in the last incomplete block (0-63)
      let t : Nat8 = (i_msg << 2) +% 4 -% i_byte;
      // p = length of padding (1-64)
      var p : Nat8 = if (t < 56) (56 -% t) else (120 -% t);
      // n_bits = length of message in bits
      let n_bits : Nat64 = ((i_block << 6) +% Nat64.fromIntWrap(Nat8.toNat(t))) << 3;

      // write padding
      writeByte(0x80);
      p -%= 1;
      while (p != 0) {
        writeByte(0x00);
        p -%= 1
      };

      // write length (8 bytes)
      // Note: this exactly fills the block buffer, hence process_block will get
      // triggered by the last writeByte
      writeByte(Nat8.fromIntWrap(Nat64.toNat((n_bits >> 56) & 0xff)));
      writeByte(Nat8.fromIntWrap(Nat64.toNat((n_bits >> 48) & 0xff)));
      writeByte(Nat8.fromIntWrap(Nat64.toNat((n_bits >> 40) & 0xff)));
      writeByte(Nat8.fromIntWrap(Nat64.toNat((n_bits >> 32) & 0xff)));
      writeByte(Nat8.fromIntWrap(Nat64.toNat((n_bits >> 24) & 0xff)));
      writeByte(Nat8.fromIntWrap(Nat64.toNat((n_bits >> 16) & 0xff)));
      writeByte(Nat8.fromIntWrap(Nat64.toNat((n_bits >> 8) & 0xff)));
      writeByte(Nat8.fromIntWrap(Nat64.toNat(n_bits & 0xff)));

      // retrieve sum
      digest[0] := Nat8.fromIntWrap(Nat32.toNat((s0 >> 24) & 0xff));
      digest[1] := Nat8.fromIntWrap(Nat32.toNat((s0 >> 16) & 0xff));
      digest[2] := Nat8.fromIntWrap(Nat32.toNat((s0 >> 8) & 0xff));
      digest[3] := Nat8.fromIntWrap(Nat32.toNat(s0 & 0xff));
      digest[4] := Nat8.fromIntWrap(Nat32.toNat((s1 >> 24) & 0xff));
      digest[5] := Nat8.fromIntWrap(Nat32.toNat((s1 >> 16) & 0xff));
      digest[6] := Nat8.fromIntWrap(Nat32.toNat((s1 >> 8) & 0xff));
      digest[7] := Nat8.fromIntWrap(Nat32.toNat(s1 & 0xff));
      digest[8] := Nat8.fromIntWrap(Nat32.toNat((s2 >> 24) & 0xff));
      digest[9] := Nat8.fromIntWrap(Nat32.toNat((s2 >> 16) & 0xff));
      digest[10] := Nat8.fromIntWrap(Nat32.toNat((s2 >> 8) & 0xff));
      digest[11] := Nat8.fromIntWrap(Nat32.toNat(s2 & 0xff));
      digest[12] := Nat8.fromIntWrap(Nat32.toNat((s3 >> 24) & 0xff));
      digest[13] := Nat8.fromIntWrap(Nat32.toNat((s3 >> 16) & 0xff));
      digest[14] := Nat8.fromIntWrap(Nat32.toNat((s3 >> 8) & 0xff));
      digest[15] := Nat8.fromIntWrap(Nat32.toNat(s3 & 0xff));
      digest[16] := Nat8.fromIntWrap(Nat32.toNat((s4 >> 24) & 0xff));
      digest[17] := Nat8.fromIntWrap(Nat32.toNat((s4 >> 16) & 0xff));
      digest[18] := Nat8.fromIntWrap(Nat32.toNat((s4 >> 8) & 0xff));
      digest[19] := Nat8.fromIntWrap(Nat32.toNat(s4 & 0xff));
      digest[20] := Nat8.fromIntWrap(Nat32.toNat((s5 >> 24) & 0xff));
      digest[21] := Nat8.fromIntWrap(Nat32.toNat((s5 >> 16) & 0xff));
      digest[22] := Nat8.fromIntWrap(Nat32.toNat((s5 >> 8) & 0xff));
      digest[23] := Nat8.fromIntWrap(Nat32.toNat(s5 & 0xff));
      digest[24] := Nat8.fromIntWrap(Nat32.toNat((s6 >> 24) & 0xff));
      digest[25] := Nat8.fromIntWrap(Nat32.toNat((s6 >> 16) & 0xff));
      digest[26] := Nat8.fromIntWrap(Nat32.toNat((s6 >> 8) & 0xff));
      digest[27] := Nat8.fromIntWrap(Nat32.toNat(s6 & 0xff));

      return Blob.fromArrayMut(digest)
    }
  }; // class SHA224

  func nat32ToByteArray(n : Nat32) : [Nat8] {
    func byte(n : Nat32) : Nat8 {
      Nat8.fromNat(Nat32.toNat(n & 0xff))
    };
    [byte(n >> 24), byte(n >> 16), byte(n >> 8), byte(n)]
  }
}
//CALL ingress go 0x4449444C0000