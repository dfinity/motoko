import Prim "mo:prim";

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

// Check that optional return type can be provided
let _ = map(ar, func x : Int = x + 1); // func return type annotation
let _ = map(ar, func x = x + 1 : Int); // body type annotation
let _ : [Int] = map(ar, func x = x + 1); // result type annotation

// Unmentioned type variables are inferred too
let _ = mapAlt(ar, func x = #inj1 x); // O=Alt<Nat, None>
let _ = mapAlt(ar, func x = #inj2 x); // O=Alt<None, Nat>

func fail<T>() : T = Prim.trap("fail");

type Compare<T> = (T, T) -> { #less; #equal; #greater };

type Foo1<T> = { var x : T };
type Foo2<T> = { x : T };

func foo1<T, R>(_ : Foo1<T>, _ : Compare<R>, _ : T -> R) : Foo1<R> = fail();
func foo2<T, R>(_ : Foo2<T>, _ : Compare<R>, _ : T -> R) : Foo2<R> = fail();

let f1 : Foo1<Nat> = { var x = 1 };
let f2 : Foo2<Nat> = { x = 1 };

func textCompare(_a : Text, _b : Text) : { #less; #equal; #greater } = #equal;
func natToText(_n : Nat) : Text = "";

let _ : Foo1<Text> = foo1(f1, textCompare, func x = natToText(x) # "!");
let _ = foo2(f2, textCompare, func x = natToText(x) # "!");

// Only fix A in the 1st round
func foo<T, A>(t : T, _ : A, _ : A -> T) : T = t;
// In the 1st round: Nat <: T  (don't fix T yet!)
// In the 2nd round: Int <: T  (solve T := Int)
let _ = foo(1, "abc", func _ = -1);

//SKIP comp
//SKIP run
//SKIP run-drun
//SKIP run-ir
//SKIP run-low
