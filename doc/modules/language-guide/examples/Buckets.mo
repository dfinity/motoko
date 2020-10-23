actor class Bucket(i : Nat) {

  type Key = Nat;
  type Value = Text;

  var map = func(k: Nat) : ?Value { return null };

  public func lookup(k : Key) : async ?Value {
    return map(k);
  };

  public func insert(k : Key, v : Value) : async () {
    let oldMap = map;
    map := func(k1 : Nat) : ?Value { if (k1 == k) ?v else oldMap(k1); };
  };

};
