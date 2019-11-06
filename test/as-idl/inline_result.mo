actor {
 type Result<Ok,Err> = {
  #ok:Ok;
  #err:Err;
 };
 public func f(x:?Nat):future Result<Nat,Text> {
  switch x {
    case (? x) {#ok x};
    case null {#err "error"};
  }
 };
 public func g(x:Result<Nat,Text>):future Result<Int,()> {
  #ok(1);
 };
}
