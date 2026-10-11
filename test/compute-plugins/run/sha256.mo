//MOC-FLAG --plugin ../../../_build/compute-plugins/sha256
import Prim "mo:⛔";

// SHA-256 known answers and padding transitions at 55/56 and 63/64 bytes.
assert Prim.sha256("") ==
  "\e3\b0\c4\42\98\fc\1c\14\9a\fb\f4\c8\99\6f\b9\24\27\ae\41\e4\64\9b\93\4c\a4\95\99\1b\78\52\b8\55";
assert Prim.sha256("abc") ==
  "\ba\78\16\bf\8f\01\cf\ea\41\41\40\de\5d\ae\22\23\b0\03\61\a3\96\17\7a\9c\b4\10\ff\61\f2\00\15\ad";
// The 32-byte specialization must match the reference implementation too.
assert Prim.sha256("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") ==
  "\3b\a3\f5\f4\3b\92\60\26\83\c1\9a\ee\62\a2\03\42\b0\84\dd\59\71\dd\d3\38\08\d8\1a\32\88\79\a5\47";
assert Prim.sha256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") ==
  "\24\8d\6a\61\d2\06\38\b8\e5\c0\26\93\0c\3e\60\39\a3\3c\e4\59\64\ff\21\67\f6\ec\ed\d4\19\db\06\c1";
assert Prim.sha256("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") ==
  "\9f\43\90\f8\d3\0c\2d\d9\2e\c9\f0\95\b6\5e\2b\9a\e9\b0\a9\25\a5\25\8e\24\1c\9f\1e\91\0f\73\43\18";
assert Prim.sha256("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") ==
  "\b3\54\39\a4\ac\6f\09\48\b6\d6\f9\e3\c6\af\0f\5f\59\0c\e2\0f\1b\de\70\90\ef\79\70\68\6e\c6\73\8a";
assert Prim.sha256("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") ==
  "\7d\3e\74\a0\5d\7d\b1\5b\ce\4a\d9\ec\06\58\ea\98\e3\f0\6e\ee\cf\16\b4\c6\ff\f2\da\45\7d\dc\2f\34";
assert Prim.sha256("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") ==
  "\ff\e0\54\fe\7a\e0\cb\6d\c6\5c\3a\f9\b6\1d\52\09\f4\39\85\1d\b4\3d\0b\a5\99\73\37\df\15\46\68\eb";
assert Prim.sha256("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") ==
  "\63\53\61\c4\8b\b9\ea\b1\41\98\e7\6e\a8\ab\7f\1a\41\68\5d\6a\d6\2a\a9\14\6d\30\1d\4f\17\eb\0a\e0";

// Fixed-signature functions retain ordinary callback and alias behavior.
func apply(hash : Blob -> Blob, data : Blob) : Blob = hash(data);
let digest : Blob -> Blob = Prim.sha256;
assert apply(digest, "abc") == Prim.sha256("abc");
let callbacks : [Blob -> Blob] = [Prim.sha256, digest];
assert callbacks[1]("abc") == callbacks[0]("abc");

// A returned result remains valid across later calls and allocations.
let saved = Prim.sha256("abc");
var i = 0;
while (i < 100) {
  assert Prim.sha256("a") ==
    "\ca\97\81\12\ca\1b\bd\ca\fa\c2\31\b3\9a\23\dc\4d\a7\86\ef\f8\14\7c\4e\72\b9\80\77\85\af\ee\48\bb";
  i += 1;
};
assert saved == digest("abc");
