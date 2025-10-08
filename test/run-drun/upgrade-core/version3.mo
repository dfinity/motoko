//MOC-FLAG --package core0 upgrade-core/core0
//MOC-FLAG --package core upgrade-core/core1
import OldMap "mo:core0/Map"; // from core0!
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Debug "mo:core/Debug";
import Prim "mo:prim";

(with migration = func (old : { map : OldMap.Map<Nat,Text> } ) :
   { map : Map.Map<Nat, Text> }
 {
   let map = Map.Map<Nat, Text>(Nat.compare);
   for ((k,v) in old.map.entries()) {
     map.add(k, v);
   };
   return { map }
 }
)
persistent actor {
  let map = Map.Map<Nat,Text>(Nat.compare);
  for (i in Nat.range(0, 10)) {
    map.add(i, Nat.toText(i));
  };
  for (e in map.entries()) {
    Debug.print(debug_show e);
  };
};
