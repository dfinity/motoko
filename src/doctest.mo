 module {
  /// I document Person
  public type Person = {
      /// Boomer?
      age: Nat;
      /// The more the merrier
      names: List.List<Text>
  };

  /// Returns the sum of x and y
  public func add (
    /// The first addend
    x: Nat,
    /// The second addend
    y: Nat
  ): (Nat, Nat) /// the sum of x and y
  {
    return 10
  }
}
