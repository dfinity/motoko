import Prim "mo:prim";

module {
    public type Equality<T> = persistent (first : T, second : T) -> Bool;
    public type Hash<T> = persistent (value : T) -> Nat;
    public type Iter<T> = { next : () -> ?T };

    public persistent class SimpleHashMap<K, V>(capacity : Nat, equal : Equality<K>, hash : Hash<K>) {
        private let table = Prim.Array_init<?(K, V)>(capacity, null);
        private var count = 0;

        public func size() : Nat {
            count;
        };

        public func put(key : K, value : V) {
            let index = hash(key) % capacity;
            switch (table[index]) {
                case null {
                    table[index] := ?(key, value);
                    count += 1;
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

        public func entries() : Iter<(K, V)> {
            object {
                var index = 0;

                private func isEmpty(index: Nat): Bool {
                    switch (table[index]) {
                        case null true;
                        case _ false;
                    }
                };

                public func next(): ?(K, V) {
                    while (index < table.size() and isEmpty(index)) {
                        index += 1;
                    };
                    if (index == table.size()) {
                        null
                    } else {
                        let result = table[index];
                        index += 1;
                        result;
                    }
                }
            }
        }
    };
};
