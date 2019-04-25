module X = {

let C = A.B

let u = C.v;

module A = {
  module B = {
    type T = ();
    let v = ();
  };
};

};