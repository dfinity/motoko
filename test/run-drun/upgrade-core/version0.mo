//MOC-FLAG --package core upgrade-core/core0
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Debug "mo:core/Debug";
import Prim "mo:prim";

persistent actor {
  let map = Map.Map<Nat,Text>(Nat.compare);
  for (i in Nat.range(0, 10)) {
    map.add(i, Nat.toText(i));
  };
  for (e in map.entries()) {
    Debug.print(debug_show e);
  };
};
