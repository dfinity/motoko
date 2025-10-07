//MOC-FLAG --package core core-stub/src

import Map "mo:core/Map";
import Text "mo:core/Text";

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

module M {
  public type Self = (Nat, Nat);
  public let i : Nat = 42;
  public func f1(_self : Self, _x : Nat, _y : Nat) {};
  public func f2(_self : Self, _xy : (Nat, Nat)) {};
  public func f3(_self : Self, _i : (implicit : (i : Nat))) {};
  public func f4(_self : Self, _xy : (Nat, Nat), _i : (implicit : (i : Nat))) {};
};
