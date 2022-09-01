let o1 = object { public type t = Int };
let o2 = object { public type t = Nat };
let o3 = object { public type t<A> = A };
let o4 = object { public type t<A> = Int };

let _ = { o1 and o1 }; // ok

let _ = { o1 and o2 }; // reject

let _ = { o3 and o3 }; // ok

let _ = { o3 and o4 }; // reject
