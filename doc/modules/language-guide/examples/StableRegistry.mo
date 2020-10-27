import Text "mo:base/Text";
import Map "mo:base/HashMap";
import Array "mo:base/Array";
import Iter "mo:base/Iter";

actor Registry {

  stable var state : [(Text, Nat)] = [];

  let dict = Map.fromIter(
    state.vals(), 10, Text.equal, Text.hash);

  public func register(name : Text) : async () {
    switch (dict.get(name)) {
      case null  {
        dict.put(name, dict.size());
      };
      case (?id) { };
    }
  };

  public func lookup(name : Text) : async ?Nat {
    dict.get(name);
  };

  system func preupgrade() {
    state := Iter.toArray(dict.entries());
  };

  system func postupgrade() {
    state := [];
  };
}
