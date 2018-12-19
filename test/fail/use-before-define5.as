// A closing actor needs to eagerly get the values
let a = actor { foo() : () { assert (x == 1) } };
let x = 1;
