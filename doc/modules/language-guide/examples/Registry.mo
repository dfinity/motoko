import Text "mo:base/Text";
import Map "mo:base/HashMap";

actor Registry {
  type Id = Nat64;
  let dict = Map.HashMap<Text,Id>(10, Text.equal, Text.hash);

  public func register(name : Text, id : Id) : async () {
    dict.put(name, id);
  };

  public func lookup(name : Text, id : Id) : async ?Id {
    dict.get(name);
  };
}
