import HashMap "hash-map";
import Migration "migration";
import Helpers "helpers";

(with migration = Migration.run)
persistent actor {
    persistent func natEqual(first : Nat, second : Nat) : Bool {
        first == second;
    };

    persistent func natHash(number : Nat) : Nat {
        number;
    };

    let _retain = {
        natHash;
        natEqual
    };

    let map = HashMap.SimpleHashMap<Text, Nat>(10, Helpers.textEqual, Helpers.textHash);        
    assert (map.size() == 2);
    assert (map.get("A") == ?1);
    assert (map.get("B") == ?2);
    assert (map.get("C") == null);
}
