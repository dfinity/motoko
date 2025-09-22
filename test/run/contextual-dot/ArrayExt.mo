module ArrayExt {
  public type Self<T> = [T];

  public func first<T>(self : [T]) : T = self[0];
  public func map<A, B>(_self : [A], _f : A -> B): [B] {
    []
  };
  public func sum(_xs : [Nat]) : Nat { 10 };
};
