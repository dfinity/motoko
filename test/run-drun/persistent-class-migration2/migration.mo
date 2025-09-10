import HashMap "hash-map";

module {
    type Actor = {
        map: HashMap.SimpleHashMap<Nat, Text>;
    };

    public func run(old: Actor): Actor {
        assert (old.map.get(1) == ?"A");
        old
    }
}
