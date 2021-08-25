object X { public type t  = Int };

object Y {
  module X { public let Y = 1; };
  type t = X.Y.t
};
