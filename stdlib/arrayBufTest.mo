import ArrayBuf "arrayBuf.mo";

let a = ArrayBuf.Make<Nat>(#empty);
let b = ArrayBuf.Make<Nat>(#capacity(1000));
let c1 = ArrayBuf.Make<Nat>(#arrayBuf(a.arrayBuf()));
let c2 = ArrayBuf.Make<Nat>(#arrayBuf(b.arrayBuf()));
let d1 = ArrayBuf.Make<Nat>(#array([1,2,3]));
let d2 = ArrayBuf.Make<Nat>(#array(a.array()));

// todo: assert stuff here about the variables defined above
