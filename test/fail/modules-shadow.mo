object X { public type t  = Int };

object Y {
  let X = 1;
  type t = X.t
};
