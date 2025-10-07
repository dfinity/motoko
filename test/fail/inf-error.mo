    type Order = {
       #less; #equal; #greater;
    };

    module Nat {
      public func compare(n : Nat, m : Nat) : Order { #equal };
    };

    module Map {
      public type Map<K,V> = {map : [(K, [var V])]};
      public type Self<K, V> = Map<K, V>;
      public func empty<K, V>() : Map<K,V> = {map= []};
      public func get<K, V>(
         map : Map<K, V>,
	 compare: (implicit : (K, K) -> Order),
  	 n : K,
	 ) : ?V {
	 ?(map.map[0].1[0])
      };

      public func set<K, V>(
         map : Map<K, V>,
	 compare: (implicit : (K, K) -> Order),
  	 n : K,
	 v : V
	 ) : Map<K, V> {
	 map
      };


   };

persistent actor {

   let peopleMap = Map.empty<Nat, Text>();
   public shared query func test() : async () {
      let person0 = peopleMap.get(Nat.compare, 1);
      let person1 = peopleMap.get(Nat.compare, 1);
      let person2 = peopleMap.get(Nat.compare, "test");
   };
}
