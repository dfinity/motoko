module ArrayExt {
  public func first<T>(self : [T]) : T = self[0];
  public func map<A, B>(self : [A], _f : A -> B): [B] {
    let _ = self;
    []
  };
  public func sum(self : [Nat]) : Nat {
    let _ = self;
    10
  };
};
