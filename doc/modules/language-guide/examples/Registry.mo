import Text "mo:base/Text";
import Map "mo:base/HashMap";

actor Registry {

  let dict = Map.HashMap<Text, Nat>(10, Text.equal, Text.hash);

  public func register(name : Text) : async () {
    switch (dict.get(name)) {
      case null {
        dict.put(name, dict.size());
      };
      case (?id) { };
    }
  };

  public func lookup(name : Text) : async ?Nat {
    dict.get(name);
  };
}
