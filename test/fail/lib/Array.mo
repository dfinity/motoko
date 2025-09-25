// Used in the test of contextual-dots
module {
  public type Self<T> = [T];

  public func first<T>(self : [T]) : T {
    self[0]
  };
}
