import Text "mo:base/Text";
import Map "mo:base/HashMap";

actor Registry {

  let map = Map.HashMap<Text, Nat>(10, Text.equal, Text.hash);

  public func register(name : Text) : async () {
    switch (map.get(name)) {
      case null {
        map.put(name, map.size());
      };
      case (?id) { };
    }
  };

  public func lookup(name : Text) : async ?Nat {
    map.get(name);
  };
};

await Registry.register("hello");
(await Registry.lookup("hello"), await Registry.lookup("world"))
