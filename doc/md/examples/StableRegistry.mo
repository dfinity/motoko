import Text "mo:base/Text";
import Map "mo:base/HashMap";
import Iter "mo:base/Iter";

persistent actor Registry {

  var entries : [(Text, Nat)] = []; // implicitly `stable`

  transient let map = Map.fromIter<Text,Nat>(
    entries.vals(), 10, Text.equal, Text.hash);

  public func register(name : Text) : async () {
    switch (map.get(name)) {
      case null  {
        map.put(name, map.size());
      };
      case (?_) { };
    }
  };

  public func lookup(name : Text) : async ?Nat {
    map.get(name);
  };

// Using preupgrade is discouraged and should be avoided if possible.
  system func preupgrade() {
    entries := Iter.toArray(map.entries());
  };

// Using postupgrade is discouraged and should be avoided if possible.
  system func postupgrade() {
    entries := [];
  };
}
