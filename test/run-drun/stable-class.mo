//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
    type Equality<T> = stable (first : T, second : T) -> Bool;
    type Hash<T> = stable (value : T) -> Nat;

    class SimpleHashMap<K, V>(capacity : Nat, equal : Equality<K>, hash : Hash<K>) {
        private let table = Prim.Array_init<?(K, V)>(capacity, null);

        public func put(key : K, value : V) {
            let index = hash(key) % capacity;
            switch (table[index]) {
                case null table[index] := ?(key, value);
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
    };

    func natEqual(first : Nat, second : Nat) : Bool {
        first == second;
    };

    func natHash(number : Nat) : Nat {
        number;
    };

    let map = SimpleHashMap<Nat, Text>(10, natEqual, natHash);
    map.put(1, "A");
    map.put(2, "B");
    assert (map.get(1) == ?"A");
    assert (map.get(2) == ?"B");
    assert (map.get(3) == null);
};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL upgrade ""
