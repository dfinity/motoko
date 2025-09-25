import Prim "mo:prim";

module {
  public type Equality<T> = (first : T, second : T) -> Bool;
  public type Hash<T> = (value : T) -> Nat;
  public type HashFn<K> = (Hash<K>, Equality<K>);

  public type HashMap<K, V, H <: HashFn<K>> = {
    var size : Nat;
    var capacity : Nat;
    table : [var ?(K, V)];
    hashFn : H;
  };
  public type Self<K, V> = HashMap<K, V, HashFn<K>>;

  public func empty<K, V, H <: HashFn<K>>(capacity : Nat, hashFn : H) : HashMap<K, V, H> {
    {
      var size = 0;
      var capacity;
      table = Prim.Array_init<?(K, V)>(capacity, null);
      hashFn;
    }
  };

  public func put<K, V>(map : Self<K, V>, key : K, value : V) {
    let (hash, equal) = map.hashFn;
    let index = hash(key) % map.capacity;
    switch (map.table[index]) {
      case null {
        map.table[index] := ?(key, value);
        map.size += 1;
      };
      case (?(otherKey, _)) {
        if (equal(key, otherKey)) {
          map.table[index] := ?(key, value);
        } else {
          Prim.trap("Hash collision"); // simplification for testing
        };
      };
    };
  };

  public func get<K, V>(map : Self<K, V>, key : K) : ?V {
    let (hash, equal) = map.hashFn;
    let index = hash(key) % map.capacity;
    switch (map.table[index]) {
      case null null;
      case (?(otherKey, value)) {
        if (equal(key, otherKey)) {
          ?value;
        } else {
          null;
        };
      };
    };
  };

  public func entries<K, V>(map : Self<K, V>) : [(K, V)] {
    let iter = map.table.values();
    Prim.Array_tabulate<(K, V)>(map.size, func(_) {
      label l loop {
        switch (iter.next()) {
          case (?(?e)) return e;
          case _ {};
        }
      };
      Prim.trap("impossible")
    });
  };

  public func natEqual(first : Nat, second : Nat) : Bool {
    first == second;
  };
  public func natHash(number : Nat) : Nat {
    number;
  };
}
