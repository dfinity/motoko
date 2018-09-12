func f(g : Int -> Int) : Int = g 5;
/* TBR
let x = f(func x = x);
let y = f(func x = x + 1);
*/


// Branch warnings.

let _ = if true true else 5;
let _ = if true 5 else 5.1;
let _ = if true {} else (new {});
let _ = if true (new {x = 5}) else (new {var x = 5});
let _ = if true 1 else (-1);  // ok
let _ = if true true else (5 : Any);  // ok

let _ = switch 0 { case 0 true; case _ 5 };
let _ = switch 0 { case 0 2 case 2 (-5); case 3 "text"; case _ () };
let _ = switch 0 { case 0 true; case _ (() : Any); };  // ok


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


// This is an error.
let _ =
{
  class C() {};
  type T = C;
  let o : T = C();
  o;
};
