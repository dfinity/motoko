let b : Blob = to_candid (?[{a=?#a}, {a=?#b{}}, {a=?(#b{})}, {a=?#a}, {a=?#c}, {a=?#a}] : ?[{a:?{#a;#b:{};#c}}]);

let o = (from_candid b) : ?[{a:?{#a}}];
//assert(o == ?[?#a, null, null, ?#a, null, ?#a]);

//SKIP run
//SKIP run-ir
//SKIP run-low