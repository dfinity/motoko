// A closing actor needs to eagerly get the values
let a = actor { public func foo() : () { assert (x == 1) } };
let x = 1;
