// actor references

let orange = actor "IC:C0FEFED00D41" : actor { fubar(n : Nat) : async Nat };

if false {
   ignore (orange.fubar(45));
}
