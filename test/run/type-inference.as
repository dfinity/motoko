// Branch warnings.

let _ = if true true else 5;
let _ = if true true else [var 5];
let _ = if true 5 else 5.1;
let _ = if true {} else (new {});
let _ = if true (new {x = 5}) else (new {var x = 5});
let _ = if true 1 else (-1);  // ok
let _ = if true true else (5 : Any);  // ok

let _ = switch 0 { case 0 true; case _ 5 };
let _ = switch 0 { case 0 true; case _ [var 5] };
let _ = switch 0 { case 0 2; case 2 (-5); case 3 "text"; case _ () };
let _ = switch 0 { case 0 true; case _ (() : Any); };  // ok


// Array warnings.

let _ = [true, 5];
let _ = [true, [var 5]];


// Local types (not) escaping.

let x =
{
  type T = {};
  let o : T = new {};
  o;
};

let y =
{
  class C() {};
  type T<A> = Int;
  let n : T<C> = 5;
  n;
};

type T<A> = Int;
let z =
{
  class C() {};
  let n : T<C> = 5;
  n;
};

class C() {};
let a =
{
  let n : T<C> = 5;
  n;
};


// Any Type

type Top = Any;
func top(top : Top) {
  let _ = [1, top];
  let _ = [true, top];
  let _ = [null, top];
  let _ = ["", top];
  let _ = [top, top];
};


// Bottom Type

{
type Bot = None;
func bot(bot : Bot) {
  //let a = bot.1;
  let b = bot.x;
  let c = bot();
  let d = bot(1, 2);
  let e = bot<Int>(5);
  let f = bot[1];
  let g = bot + bot * bot;
  for (x in bot) {};
};
};

// This was an error when classes were subtypes of their
// representation, but with structural typing throughout it is
// no longer one
{
  let _ =
  {
    class C() {};
    type T = C;
    let o : T = C();
    o;
  };
};


// Function types.

func f(h : Int -> Int) : Int = h 5;
func g(h : (Int, Int) -> Int) : Int = h(5, 6);
func k(h : {#A; #B : Int} -> Int) : Int = h(#B 9);

let _ = f(func x = x);
let _ = f(func x = x + 1);
let _ = f(func x = abs(x + 1));
let _ = f(func x = -1);
let _ = g(func p = abs(p.0));
let _ = g(func(x, y) = x + y);
let _ = g(func(x, y) = abs x);
let _ = k(func(#A or #B _) = 0);

let _ = f(func x : Int = x);
let _ = f(func x : Int = x + 1);
let _ = f(func x : Nat = abs(x + 1));
let _ = f(func x : Int = 0);
let _ = g(func p : Nat = abs(p.0));
let _ = g(func(x, y) : Int = x + y);
let _ = g(func(x, _) : Nat = abs x);
let _ = k(func(#A or #B _) : Nat = 0);

let _ = f(func(x : Int) : Int = x);
let _ = f(func(x : Int) : Int = x + 1);
let _ = f(func(x : Int) : Nat = abs(x + 1));
let _ = f(func(x : Any) : Int = 0);
let _ = g(func(p : (Int, Any)) : Nat = abs(p.0));
let _ = g(func(x : Int, y : Int) : Int = x + y);
let _ = g(func(x : Int, _ : Any) : Nat = abs x);
let _ = g(func((x, _) : (Int, Any)) : Nat = abs x);
let _ = k(func(#A or #B (_ : Any)) : Nat = 0);
let _ = k(func((#A or #B _) : {#A; #B : Any}) : Nat = 0);

let _ = f(func<>(x : Int) : Int = x);
let _ = f(func<>(x : Int) : Int = x + 1);
let _ = f(func<>(x : Int) : Nat = abs(x + 1));
let _ = f(func<>(x : Any) : Int = 0);
let _ = g(func<>(p : (Int, Any)) : Nat = abs(p.0));
let _ = g(func<>(x : Int, y : Int) : Int = x + y);
let _ = g(func<>(x : Int, y : Any) : Nat = abs x);
let _ = g(func<>((x, _) : (Int, Any)) : Nat = abs x);
let _ = k(func<>(#A or #B (_ : Any)) : Nat = 0);
let _ = k(func<>((#A or #B _) : {#A; #B : Any}) : Nat = 0);
