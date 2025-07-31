import Text "mo:core/Text";
import Map "mo:core/Map";

actor Registry {

  let map = Map.empty<Text, Nat>();

  public func register(name : Text) : async () {
    switch (map.get(name)) {
      case null {
        map.put(name, map.size());
      };
      case (?_) { };
    }
  };

  public func lookup(name : Text) : async ?Nat {
    map.get(name);
  };
};

await Registry.register("hello");
(await Registry.lookup("hello"), await Registry.lookup("world"))
