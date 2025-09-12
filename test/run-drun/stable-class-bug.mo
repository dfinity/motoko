//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

// counter-example, demonstrating unsoundness of promoting a generic stable function to a generic non-stable function.
// Type arguments are restricted to stable types for stable functions, but
// unrestricted for non-stable functions, so you can avoid the restriction
// by promoting the function to a non-stable super type before application.

//
// Possible fixes:
// * Only allow type parameters to range over stable types, both for local and stable functions.
//   Probably too draconian. Not backwards compatible.
// * Add a type parameter sort for stable type (similar to equality type parameters in SML/restricted type classes).
// * Consider a stable-sorted function type stable only if, when applied to stable
//   type parameters, its arguments and results are stable types too. That might
//   rule out the map field below, because it's type is actually not stable,
//   eventhough it contains just stable-sorted functions.

/*
[nix-shell:~/motoko/test/run-drun]$ EXTRA_MOC_ARGS=--enhanced-orthogonal-persistence ../run.sh -d  stable-class-bug.mo
WARNING: Could not run ic-ref-run, will skip running some tests
stable-class-bug: [tc] [comp] [comp-ref] [valid] [valid-ref] [drun-run]
All tests passed.

[nix-shell:~/motoko/test/run-drun]$ moc --enhanced-orthogonal-persistence --stable-types stable-class-bug.mo

[nix-shell:~/motoko/test/run-drun]$ more stable-class-bug.most
// Version: 1.0.0
actor {
  stable map :
    {get : stable Nat -> ?(() -> Text); put : stable (Nat, () -> Text) -> ()}
};
*/

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

// correctly rejected, V is non-stable
//    let map = SimpleHashMap<Nat, () -> Text>(10, natEqual, natHash);
//    map.put(1, func () = "A");
//    map.put(2, func () = "B");

    // a non-stable super-type of the class constructor type
    type Super = <K, V>(capacity : Nat, equal : Equality<K>, hash : Hash<K>) ->
    { put : stable (K, V) -> ();
      get : stable K -> ?V
    };

    transient let SuperHashMap = SimpleHashMap : Super;
    // incorrectly allowed, though V is non-stable
    let map = SuperHashMap<Nat, () -> Text>(10, natEqual, natHash);
    map.put(1, func () = "A");
    map.put(2, func () = "B");
//    assert (map.get(1)() == ?"A");
//    assert (map.get(2) == ?"B");
//    assert (map.get(3) == null);
};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL upgrade ""
