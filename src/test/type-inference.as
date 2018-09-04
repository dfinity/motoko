func f(g : Int -> Int) : Int = g 5;
/* TBR
let x = f(func x = x);
let y = f(func x = x + 1);
*/


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
