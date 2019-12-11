// actor literals

let _ = actor "ic:C0FEFED00D";
let _ = actor "ic:C0FEFED00D" : actor { foo(n : Nat) : async Nat };
