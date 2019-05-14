module X = {

module A = {
  module B = {
    type T = ();
    let v  = ()
  };
};

let C = A.B;

type U = C.T;

let u = C.v;

};


