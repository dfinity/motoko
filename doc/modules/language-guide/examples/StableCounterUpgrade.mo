actor Counter {

  stable var value = 0;

  public func inc() : async Nat {
    value += 1;
    return value;
  };

  punc func reset() : async () {
    value := 0;
  }
}
