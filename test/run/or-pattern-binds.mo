// (FAILS) let (#a a1) or (#b b1) = #a 5;
let (#a a2) or (#b a2) : { #a : Nat; #b : Nat } = #a 5;
// (FAILS) let (#a a3) or (#b a3) : { #a : Nat; #b : Char } = #a 5;
// (should work, fails currently)
// let (#a a4) or (#b a4) = #a 5;
// let (#a a5) or (#b a5) = #b 5;
