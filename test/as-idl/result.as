type Result<Ok,Err> = {
  #ok:Ok;
  #err:Err;
};
actor {
 public func f(x:?Nat):async Result<Nat,Text> {
  switch x {
    case (? x) {#ok x};
    case null {#err "error"};
  }
 };
}
