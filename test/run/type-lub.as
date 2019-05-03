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

let poly_funcs = [ func<A> (a : [Int]) : Nat = a.len()
                 , func<A> (a : [Nat]) : Int = -42
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
let mut_arrs = [[var 42], [var 25], [77]]; // boring

// TODO(gabor), mutable fields, see fail/type-inference.as:13

// TODO(gabor), Shared

let sh : Shared = 42;
let shareds = [sh, 77, [1, 2, 3]];
// let shared2s = [77, [1, 2, 3], sh]; // CAVEAT: order dependency in lub!

let shared_funcs = [ func (a : Int) : Int = a
                   , func (a : Shared) : Nat = 42
                   ];


// recursive objects

// { need global types due to https://dfinity.atlassian.net/browse/AST-34
type A = {x : A};
type B = {x : B};

func f(v : {x : {x : B}; b : B}, x : A, y : B, z : {x : B; a : A}) : [A] { ignore([v, x, y, z]); [v, x, y, z] };
// };

// {
type A1 = {x : B1};
type B1 = {x : A1};

func f1(x : A1, y : B1) : [A1] { ignore([x, y]); [x, y] };
// };

type O = ?O;
type P = ?P;

type Q = ?R;
type R = ?S;
type S = ?Q;

func g(o : O, p : P, q : Q, r : R) : [O] { ignore([o, p, q, r]); [o, p, q, r] };


type U = { #a : U; #b : Int };
type V = { #a : V; #b : Nat };

func v0(u : U, v : V, w : { #a : { #a : V; #b : Nat }; #b : Nat }) : [U] { ignore([u, v, w]); [u, v, w] };


type G = (Nat, ?G);
type H = (Int, ?H);

func g0(g : G, h : H) : [H] { ignore([g, h]); [g, h] };


type K = [K];
type L = [L];

func k0(k : K, l : L) : [L] { ignore([k, l]); [k, l] };


type K1 = [?(Nat, K1)];
type L1 = [?(Int, L1)];

func k1(k : K1, l : L1) : [L1] { ignore([k, l]); [k, l] };


/*
type M = [var ?M];
type N = [?N];

func m0(m : M, n : N) : [M] { ignore([m, n]); [m, n] }
*/
