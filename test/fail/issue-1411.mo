import M "lib/M";

let Z : M.F = M.F();

type T = Z.T;  //rejected: cannot infer type of forward variable reference Z
let x  = Z.x;  //ok