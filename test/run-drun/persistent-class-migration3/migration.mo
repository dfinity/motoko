import HashMap "hash-map";
import Prim "mo:prim";
import Helpers "helpers";

module {
    type OldActor = {
        map: HashMap.SimpleHashMap<Nat, Text>;
    };

    type NewActor = {
        map: HashMap.SimpleHashMap<Text, Nat>;
    };

    public func run(old: OldActor): NewActor {
        let newMap = HashMap.SimpleHashMap<Text, Nat>(10, Helpers.textEqual, Helpers.textHash);
        Prim.debugPrint("OLD SIZE: " # debug_show(old.map.size()));
        for ((key, value) in old.map.entries()) {
            Prim.debugPrint(debug_show(key) # " " # value);
            newMap.put(value, key);
        };
        { map = newMap }
    }
}
