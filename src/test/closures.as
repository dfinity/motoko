func add1 (x : Nat) : Nat = x + 1;

/* Storing functions */
let fs = [add1, add1, add1];

assert(fs[0](fs[1](fs[2](1))) == 4);
