type Order = {#less; #equal; #greater};
module Map {
  public type Self<K, V> = [(K, V)];
  public func empty<K, V>() : Self<K, V> { [] };
  public func add<K, V>(self : Self<K, V>, compare : (implicit : (K, K) -> Order), key : K, value : V) {};
  public func remove<K, V>(self : Self<K, V>, compare : (implicit : (K, K) -> Order), key : K) {};
};

do {
  func compare(n : Nat, m: Nat) : Order {
    if (n < m) #less
    else if (n == m) #greater
    else #equal;
  };

  let map = Map.empty<Nat, Text>();
  map.add(10, "hello"); // Infers fine
  map.remove(compare, 10); // ok
  map.remove(10); // fails
}
