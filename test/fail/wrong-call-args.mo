//MOC-FLAG --package core ../core-stub/src

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
  let arg = (1, Text.compare);
  func main1() {
    foo(arg);
  };
  func main2() {
    foo arg;
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

module FuncWithSingleArgTuple {
  func foo<K>(_ : (n : Nat, compare : (implicit : (K, K) -> Types.Order))) {};
  func main() {
    let arg = (1, Text.compare);
    foo(arg);
  };
};

module ImplicitTuple {
  func foo(tup : (implicit : (Nat, ()))) {};
  let tup = (1, ());
  func main() {
    foo(tup);
    foo tup;
    foo();
  };
};

module SimpleNoDotNoImplicit {
  func f0() {};
  func f1<A>(_ : A) {};
  func f12<A, B>(_ : (A, B)) {};
  func f2<A, B>(_ : A, _ : B) {};
  let u = ();
  let n2 = (1, u);
  func correct() {
    f0();
    f0(u);
    f0 u;

    f1(1);
    f1 1;
    f1(());
    f1();
    f1(u);
    f1 u;
    f1(1, 2); // this should also be fine
    f1(1, 2, 3); // any arg is fine

    f12(1, 2);
    f12(n2);
    f12 n2;

    f2(1, 2);
    f2(n2);
    f2 n2;
  };
  func errors() {
    f0(1);
    f0(1, 2);

    f12();
    f12(1);
    f12(1, 2, 3);

    f2();
    f2(1);
    f2(1, 2, 3);
  };
};
