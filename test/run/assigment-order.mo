// assignment order checks

var o = "";

// Array
([var 1, 2, do {o #= "a"; 3}])[do {o #= "b"; 1}] := do {o #= "c"; 42};

assert o == "abc";


// Object/record
