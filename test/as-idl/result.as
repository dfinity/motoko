type Result<Ok,Err> = {
  #ok:Ok;
  #err:Err;
};
type Result2<Ok> = {
  #ok:Ok;
  #err:Result<Ok,Text>;
};
type Result3<Ok> = {
  #ok:Ok;
  #err:Result2<Ok>;
};
actor {
 public func f(x:?Nat):async Result<Nat,Text> {
  switch x {
    case (? x) {#ok x};
    case null {#err "error"};
  }
 };
 public func g(x:Result3<()>):async Result2<Nat> {
  #ok(1);
 };
}
