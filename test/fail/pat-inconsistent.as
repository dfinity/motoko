// Okay joins
let _ = func(1 or 2) {};
let _ = func(1 or -1) {};
let _ = func(1 or (_ : Int)) {};
let _ = func((1 : Int) or 2) {};
let _ = func({a = _ : Int} or {a = _ : Nat}) {};
let _ = func((_ : Int -> Int) or (_ : Nat -> Nat)) {};
let _ = func((-1, 2) or (2, -1)) {};

// Bogus joins
{ let _ = func(1 or 1.0) {} };
{ let _ = func(1 or "") {} };
{ let _ = func(true or "") {} };
{ let _ = func(1 or ()) {} };
{ let _ = func(1 or {}) {} };
{ let _ = func(1 or (_ : Text)) {} };
{ let _ = func({a = _ : Int} or {b = _ : Nat}) {} };
{ let _ = func((_ : Int8) or (_ : Nat8)) {} };
{ let _ = func(("", 1) or ("", 1.0)) {} };
{ let _ = func((_ : Int16, "") or (_ : Nat16, "")) {} };

{ let _ = func(1 or (_ : Any)) {} };
{ let _ = func((_ : Shared) or 1.4) {} };
{ let _ = func(() or (_ : Any)) {} };
{ let _ = func<A>("" or (_ : A)) {} };
{ let _ = func((_ : Any) or (_ : [Nat])) {} };
{ let _ = func((_ : Any) or (_ : () -> ())) {} };
{ let _ = func(("", 1) or ("", _ : Any)) {} };
