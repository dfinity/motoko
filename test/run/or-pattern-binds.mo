// (FAILS) let (#a a1) or (#b b1) = #a 5;
// (should be good) let (#a a2) or (#b a2) : { #a : Nat; #b : Nat } = #a 5;
let (#a a3) or (#b a3) : { #a : Nat; #b : Char } = #a 5;
