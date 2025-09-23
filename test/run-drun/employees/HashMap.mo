import Prim "mo:prim";

module {
  public type Equality<T> = persistent (first : T, second : T) -> Bool;
  public type Hash<T> = persistent (value : T) -> Nat;

  public persistent class SimpleHashMap<K, V>(capacity : Nat, equal : Equality<K>, hash : Hash<K>) {
    private var size = 0;
    private let table = Prim.Array_init<?(K, V)>(capacity, null);

    public func put(key : K, value : V) {
      let index = hash(key) % capacity;
      switch (table[index]) {
        case null {
          table[index] := ?(key, value);
          size += 1;
        };
        case (?(otherKey, _)) {
          if (equal(key, otherKey)) {
            table[index] := ?(key, value);
          } else {
            Prim.trap("Hash collision"); // simplification for testing
          };
        };
      };
    };

    public func get(key : K) : ?V {
      let index = hash(key) % capacity;
      switch (table[index]) {
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

    public func entries() : [(K, V)] {
      let iter = table.values();
      Prim.Array_tabulate<(K, V)>(size, func(_) {
        label l loop {
          switch (iter.next()) {
            case (?(?e)) return e;
            case _ {};
          }
        };
        Prim.trap("impossible")
      });
    };
  };

  public persistent func natEqual(first : Nat, second : Nat) : Bool {
    first == second;
  };

  public persistent func natHash(number : Nat) : Nat {
    number;
  };

}
