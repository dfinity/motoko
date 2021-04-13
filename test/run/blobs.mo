import Prim "mo:⛔";

Prim.debugPrint (debug_show ("" : Blob));
Prim.debugPrint (debug_show ("\00\01\02" : Blob));
Prim.debugPrint (debug_show ("\FF" : Blob));
Prim.debugPrint (debug_show ("\u{FF}" : Blob));
Prim.debugPrint (debug_show ("☃" : Blob));

assert (("":Blob) == ("":Blob));
assert (("\00":Blob) > ("":Blob));
assert (("\00":Blob) >= ("":Blob));
assert (("\00":Blob) < ("\01":Blob));
assert (("\00":Blob) <= ("\01":Blob));

do {
let i1 = ("\00\01☃":Blob).vals();
switch(i1.next()) {
  case (?b) { assert (b == (0:Nat8)); };
  case null { assert false; };
};
switch(i1.next()) {
  case (?b) { assert (b == (1:Nat8)); };
  case null { assert false; };
};
switch(i1.next()) {
  case (?b) { assert (b == (0xe2:Nat8)); };
  case null { assert false; };
};
switch(i1.next()) {
  case (?b) { assert (b == (0x98:Nat8)); };
  case null { assert false; };
};
switch(i1.next()) {
  case (?b) { assert (b == (0x83:Nat8)); };
  case null { assert false; };
};
switch(i1.next()) {
  case (?b) { assert false; };
  case null {};
};
};
