import { cons; nil } = "lib/ListM";

//type stack = List<Int>;
let s = cons(1, nil());
let u = cons<Int>(2, nil<Int>());
