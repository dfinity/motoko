module M1 {
  public type Map<K, V> = { map : [(K, [var V])] };
  public func empty<K, V>() : Map<K, V> = { map = [] };
  public func onMap<K, V, T>(k : Map<K, V> -> T) : T = k(empty());
};

module M2 {
  public type Map<K, V> = { map : [var (K, [var V])] };
  public func empty<K, V>() : Map<K, V> = { map = [var] };
  public func onMap<K, V, T>(k : Map<K, V> -> T) : T = k(empty());
};

module Main {
  func check<T>(t : T, _ : T -> ()) {};
  public func showRecord(r : {m1 : M1.Map<Nat, Text>; m2 : M2.Map<Nat, Text>}) {
    let _ : {m2 : M1.Map<Nat, Text> } = r;
  };
  public func showBimatch(r : {m1 : M1.Map<Nat, Text>; m2 : M2.Map<Nat, Text>}) {
    check(r.m1, func(m : M2.Map<Nat, Text>) = ());
  };
};
