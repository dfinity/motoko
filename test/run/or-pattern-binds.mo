import { debugPrint } = "mo:⛔";
// (FAILS) let (#a a1) or (#b b1) = #a 5;
let (#a a2l) or (#b a2l) : { #a : Nat; #b : Nat } = #a 5;
let (#a a2r) or (#b a2r) : { #a : Nat; #b : Nat } = #b 7;
// (FAILS) let (#a a3) or (#b a3) : { #a : Nat; #b : Char } = #a 5;
// (should work, fails currently)
// let (#a a4) or (#b a4) = #a 5;
// let (#a a5) or (#b a5) = #b 5;

debugPrint (debug_show {a2l; a2r})
