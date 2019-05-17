module X = { type t  = Int };

module Y = {
  let X = 1;
  type t = X.t
};

