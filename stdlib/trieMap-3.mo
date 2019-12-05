import T "trie.mo";
import P "prelude.mo";
import Hash "hash.mo";

// Done: Task: Implement `insert`.
// Task: Test `insert` function.
module {
public class Map<K,V> (isEq:(K, K) -> Bool, hashOf: K -> T.Hash) {

  var map = T.empty<K, V>();

  // update map mutably; return old element, if any
  public func insert(k:K, v:V) : ?V {
    let keyObj = {key=k; hash=hashOf(k);};
    let (map2, ov) = T.insert<K,V>(map, keyObj, isEq, v);
    map := map2;
    ov
  };

  public func add(k:K, v:V) =
    ignore insert(k, v);
};

module FlowerTest {

  // define a type Flower and use it as a value type in some maps.
  // todo: make it easier to define custom hash functions,
  // so that Flower can easily become a key type too.

  func natIsEq(x:Nat,y:Nat):Bool { x == y };
  func textIsEq(x:Text,y:Text):Bool { x == y };

  type Flower = {
    color:{#red;#blue};
    mass: Nat
  };


  public func testInsert() {

    let byTime = Map<Nat, [Flower]>(natIsEq, Hash.Hash.hashOfInt);
    let byName = Map<Text, [Flower]>(textIsEq, Hash.Hash.hashOfText);

    let f1 = {color=#red;  mass=1};
    let f2 = {color=#blue; mass=2};
    let f3 = {color=#red; mass=3};

    let io1 = byTime.insert(20191203, [f1, f2]);
    let io2 = byTime.insert(20191205, [f3]);

    let io3 = byName.insert("pretty", [f1, f3]);
    let io4 = byName.insert("ugly", [f2]);

    byTime.add(20191203, [f1, f2]);
    byTime.add(20191205, [f3]);

    byName.add("pretty", [f1, f3]);
    byName.add("ugly", [f2]);

    // todo: print these tables in a nice way.
  };
  
  
  public func testAdd() {

    let byTime = Map<Nat, [Flower]>(natIsEq, Hash.Hash.hashOfInt);
    let byName = Map<Text, [Flower]>(textIsEq, Hash.Hash.hashOfText);

    let f1 = {color=#red;  mass=1};
    let f2 = {color=#blue; mass=2};
    let f3 = {color=#red; mass=3};

    byTime.add(20191203, [f1, f2]);
    byTime.add(20191205, [f3]);

    byName.add("pretty", [f1, f3]);
    byName.add("ugly", [f2]);

    // todo: print these tables in a nice way.
  };

  public func testAll() {
    testInsert();
    testAdd();
  }
}
}
