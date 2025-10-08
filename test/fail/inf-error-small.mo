type Order = {
  #less; #equal; #greater;
};

module Any {
  public func compare(n : Any, m : Any) : Order { #equal };
};

module Nat {
  public func compare(n : Nat, m : Nat) : Order { #equal };
};

module Text {
  public func compare(n : Text, m : Text) : Order { #equal };
};

module Map {
  public type Map<K,V> = {map : [(K, [var V])]};
  public type Self<K, V> = Map<K, V>;
  public func empty<K, V>() : Map<K,V> = { map= []};

  public func get<K, V>(
    map : Map<K, V>,
    compare: (implicit : (K, K) -> Order),
    n : K)
  : ?V {
    null
  };

  public func set<K, V>(
    map : Map<K, V>,
    compare: (implicit : (K, K) -> Order),
    n : K,
    v : V)
  : Map<K, V> {
    map
  };
};

persistent actor {
  let peopleMap = Map.empty<Nat, Text>();

  func test1() {
     ignore peopleMap.get("text") : Text; // bad
     ignore peopleMap.get(Nat.compare, "text") : Text; // bad

     ignore peopleMap.get(peopleMap, "text") : Text; // bad
     ignore peopleMap.get(peopleMap, Nat.compare, "text") : Text; // bad
     ignore peopleMap.get(peopleMap, "text") : Text; // bad
  };
}
