func mapMono<T>(ar : [T], _f : T -> T) : [T] = ar;
func mapMonoTuple<T>(ar : [T], _t : (T -> T, T -> T)) : [T] = ar;
func filter<T>(array : [T], _f : T -> Bool) : [T] = array;
func filterTuple<T>(ar : [T], _t : (T -> Bool, T -> Bool)) : [T] = ar;

func forEach<T>(_ar : [T], _f : T -> ()) {};

func map<I, O>(_ar : [I], _f : I -> O) : [O] = [];
type Alt<A, B> = { #inj1 : A; #inj2 : B };
func mapAlt<I, O1, O2>(_ar : [I], _f : I -> Alt<O1, O2>) : [Alt<O1, O2>] = [];

let ar = [1, 2, 3];

// Use `ar` to infer all type variables (just T), check lambda after
let _ = mapMono(ar, func x = x + 1);
let _ = mapMonoTuple(ar, (func x = x + 1, func x = x + 1));
let _ = filter(ar, func x = x > 1);
let _ = filterTuple(ar, (func x = x > 1, func x = x > 1));
let _ = forEach(ar, func x = ());

// Infer I=Nat from ar, then infer O from the body
let _ = map(ar, func x = x + 1); // O=Nat
let _ = map(ar, func x = debug_show x # "!"); // O=Text

// Unmentioned type variables are inferred too
let _ = mapAlt(ar, func x = #inj1 x); // O=Alt<Nat, None>
let _ = mapAlt(ar, func x = #inj2 x); // O=Alt<None, Nat>

let _ = mapAlt(ar, func x = if (x > 1) { #inj1 x } else { #inj2() }); // O=Alt<Nat, ()>

//SKIP comp
//SKIP run
//SKIP run-drun
//SKIP run-ir
//SKIP run-low
