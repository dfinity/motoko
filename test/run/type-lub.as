let opts = [null, ?42, ?-25];
let nulls = [null, null];

let incompatible_objs = [new {a = 42}, new {b = 42}];
let objs = [new {a = 42}, new {b = 42; a = 1}, new {a = -25}];
let obj_arrs = [new {len() : Nat = 42}, [1, 2, 3]];
let obj_texts = [new {len() : Nat = 42}, "hello"];
let arr_texts = [[1, 2, 3], "hello"];
let obj_arr_texts = [new {len() : Nat = 42}, [1, 2, 3], "hello"];

let tups = [(12, -1), (-42, 25)];
let tup1s = [(-1,), 25];

let arrs = [[-42], [25]];

// TODO(gabor), mutable arrays
// TODO(gabor), mutable fields, see fail/type-inference.as:13
