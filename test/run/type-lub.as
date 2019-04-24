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

let incompatible_funcs = [ func (a : [Int]) : Nat = a.len()
                         , func (a : ()) : Int = -42
                         ];
let funcs = [ func (a : [Int]) : Nat = a.len()
            , func (a : [Nat]) : Int = -42
            ];

let obj_arr_funcs = [ func (a : [Int]) : Nat { printInt (a[0]); a.len() }
                    , func (a : {len : () -> Nat}) : Nat = a.len()
                    ];

let obj_text_funcs = [ func (a : Text) : Nat = a.len()
                     , func (a : {len : () -> Nat}) : Nat = a.len()
                     ];

let arr_text_funcs = [ func (a : Text) : Nat = a.len()
                     , func (a : [Char]) : Nat { printChar (a[0]); a.len() }
                     ];

let variant_funcs = [ func (a : {#foo; #bar}) { switch a { case (#foo) (); case (#bar) () } }
                    , func (a : {#baz; #bar}) { switch a { case (#baz) (); case (#bar) () } }
                    ];

// TODO(gabor), mutable arrays
// TODO(gabor), mutable fields, see fail/type-inference.as:13
