type Result<Ok,Err> = {
  #ok:Ok;
  #err:Err;
};
type Result2<Ok> = {
  #ok:Ok;
  #err:Result<Text,Ok>;
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
 public func g(x:Result3<Text>):async Result2<Int> {
  #ok(1);
 };
}
