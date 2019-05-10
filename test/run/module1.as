{
module X =
{ type T = Int;
  let x:T = 1;
};

type T = X.T;
let x = X.x;
let y = X.x + 1;
};


{
module X = {
  type T<A> = (A,A);
  let x:T<Int> = (1,2);
};

type T<A> = X.T<A>;

let x = X.x;
let (x1,x2) : T<Int> = X.x;
assert (x1 == 1);
assert (x2 == 2);

};


{
module X = {
  module X = {
    type T<A> = (A,A);
    let x:T<Int> = (1,2);
  };
};
type T<A> = X.X.T<A>;

let x = X.X.x;
let (x1,x2) : T<Int> = X.X.x;
assert (x1 == 1);
assert (x2 == 2);
};

{
module X = {
  module X = {
    type T<A> = (A,A);
    let x:T<Int> = (1,2);
  };
};

module Y = {
  type T<A> = X.X.T<A>;

  let x = X.X.x;
  let (x1,x2) : T<Int> = X.X.x;
  let _ = assert (x1 == 1);
  let _ = assert (x2 == 2);
};

type U<A> = (X.X.T<A>,Y.T<A>);

};
