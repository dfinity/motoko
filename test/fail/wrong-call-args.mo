//MOC-FLAG --package core core-stub/src

import Map "mo:core/Map";
import Text "mo:core/Text";
import Types "mo:core/Types";

let map = Map.empty<Text, Text>();

module Ok {
  func singleArgAsMany1() {
    let arg = (map, Text.compare, "abc");
    let _ = Map.get(arg);
  };
  func singleArgAsMany2() {
    let arg = (map, Text.compare, "abc");
    let _ = Map.get arg;
  };
};

module BreakingChange {
  func foo<K>(_ : Nat, compare : (implicit : (K, K) -> Types.Order)) {
    ignore compare;
  };
  func main() {
    let arg = (1, Text.compare);
    foo(arg);
  };
};

module WithDot {
  func expected1Got0() {
    map.get(); // produces: hole <: (compare : (implicit : (K, K) -> Order), key : K)
  };
  func missingValueCase() {
    let key = "abc";
    map.add(Text.compare, key); // missing value, low priority issue: Text.compare should not appear
  };
};

module WithoutDot {
  func expected1Got0() {
    Map.get(map); // here `map` is considered as a tuple of all arguments
  };
  func expected0Got1Explicit() : Map.Map<Text, Text> {
    Map.empty<Text, Text>(Text.compare);
  };
  func expected0Got1() : Map.Map<Text, Text> {
    Map.empty(Text.compare);
  };
  func missingValueCase() {
    let key = "abc";
    Map.add(map, Text.compare, key); // missing value, low priority issue: Text.compare should not appear
  };
};
