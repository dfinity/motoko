import HashMap "hash-map";

persistent actor {
    persistent func natEqual(first : Nat, second : Nat) : Bool {
        first == second;
    };

    persistent func natHash(number : Nat) : Nat {
        number;
    };

    let map = HashMap.SimpleHashMap<Nat, Text>(10, natEqual, natHash);
    map.put(1, "A");
    map.put(2, "B");
    assert (map.size() == 2);
    assert (map.get(1) == ?"A");
    assert (map.get(2) == ?"B");
    assert (map.get(3) == null);
}
