let (#a a1) or (#b a1) : { #a : Nat; #b : Char } = #a 5;
let (#a (a2 : Nat)) or (#b a2) : { #a : Nat; #b : Int } = #b 5;
let (#a (a3 : Nat)) or (#b a3) = #b (5 : Int);
