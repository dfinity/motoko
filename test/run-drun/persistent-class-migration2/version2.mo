import HashMap "hash-map";
import Migration "migration";

(with migration = Migration.run)
persistent actor {
    let state = {
        var counter = 0;
    };

    persistent func natEqual(first : Nat, second : Nat) : Bool {
        first == second;
    };

    persistent func natHash(number : Nat) : Nat {
        state.counter += 1;
        number;
    };

    let map = HashMap.SimpleHashMap<Nat, Text>(10, natEqual, natHash);
    assert (map.size() == 2);
    assert (map.get(1) == ?"A");
    assert (map.get(2) == ?"B");
    assert (map.get(3) == null);
}