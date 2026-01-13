func varMap<T, U>(_ar : [var T], _f : T -> U) : [var U] = [var];

let ar = [1, 2, 3];

// The error message should not mention `Type.Var`s
let _ = varMap(ar, func x = x : Int);
