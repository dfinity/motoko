type List<T> = ?(T, List<T>);
actor {
  stable ln : List<Nat>;
  stable lnn : List<(Nat, Nat)>;
};
