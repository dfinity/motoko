var x : Int = 0;
assert (x == 0);

let _ = {a = x := 1; b = x := 2};
assert (x == 2);

let _ = {b = x := 3; a = x := 4};
assert (x == 4);
