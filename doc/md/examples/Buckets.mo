import Nat "mo:base/Nat";
import Map "mo:base/OrderedMap";

persistent actor class Bucket(n : Nat, i : Nat) {

  type Key = Nat;
  type Value = Text;

  transient let keyMap = Map.Make<Key>(Nat.compare);

  var map : Map.Map<Key, Value> = keyMap.empty();

  public func get(k : Key) : async ?Value {
    assert((k % n) == i);
    keyMap.get(map, k);
  };

  public func put(k : Key, v : Value) : async () {
    assert((k % n) == i);
    map := keyMap.put(map, k, v);
  };

};
