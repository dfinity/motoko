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
  switch top {
    case 1 ();
    case true ();
    case null ();
    case _ ();
  };
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

func f(g : Int -> Int) : Int = g 5;

let _ = f(func x : Int = x);
let _ = f(func x : Int = x + 1);
