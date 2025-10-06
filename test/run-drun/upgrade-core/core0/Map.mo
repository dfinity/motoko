import ImperativeMap "imperative/Map";
import ImperativeIter "imperative/Iter";
import Iter "Iter";
import PureMap "pure/Map";
import Order "Order";
import Array "Array";

module {
  public type PersistentCompare<K> = persistent(K, K) -> Order.Order;
  type TransientCompare<K> = (K, K) -> Order.Order;

  public persistent class Map<K, V>(comparison : PersistentCompare<K>) {
    let inner = ImperativeMap.empty<K, V>();

    public func toPure() : PureMap.Map<K, V> {
      PureMap.fromIter(ImperativeMap.entries(inner), comparison : TransientCompare<K>)
    };

    public func toImperative() : ImperativeMap.Map<K, V> {
      ImperativeMap.clone(inner)
    };

    public func toArray() : [(K, V)] {
      entries().toArray()
    };

    public func toVarArray() : [var (K, V)] {
      Array.toVarArray(toArray())
    };

    public func isEmpty() : Bool {
      ImperativeMap.isEmpty(inner)
    };

    public func size() : Nat {
      ImperativeMap.size(inner)
    };

    public func add(key : K, value : V) {
      ImperativeMap.add(inner, comparison : TransientCompare<K>, key, value)
    };

    public func containsKey(key : K) : Bool {
      ImperativeMap.containsKey(inner, comparison : TransientCompare<K>, key)
    };

    public func get(key : K) : ?V {
      ImperativeMap.get(inner, comparison : TransientCompare<K>, key)
    };

    public func remove(key : K) {
      ImperativeMap.remove(inner, comparison : TransientCompare<K>, key)
    };

    public func clone() : Map<K, V> {
      fromImperative(inner, comparison)
    };

    public func clear() {
      ImperativeMap.clear(inner)
    };

    public func equal(other : Map<K, V>, equalValue : (V, V) -> Bool) : Bool {
      ImperativeMap.equal<K, V>(inner, other.internalMap(), comparison : TransientCompare<K>, equalValue)
    };

    public func compare(other : Map<K, V>, compareValue : (V, V) -> Order.Order) : Order.Order {
      ImperativeMap.compare(inner, other.internalMap(), comparison : TransientCompare<K>, compareValue)
    };

    public func maxEntry() : ?(K, V) {
      ImperativeMap.maxEntry(inner)
    };

    public func minEntry() : ?(K, V) {
      ImperativeMap.minEntry(inner)
    };

    public func entries() : Iter.Iter<(K, V)> {
      Iter.Iter(ImperativeMap.entries(inner))
    };

    public func reverseEntries() : Iter.Iter<(K, V)> {
      Iter.Iter(ImperativeMap.reverseEntries(inner))
    };

    public func keys() : Iter.Iter<K> {
      Iter.Iter(ImperativeMap.keys(inner))
    };

    public func values() : Iter.Iter<V> {
      Iter.Iter(ImperativeMap.values(inner))
    };

    public func forEach(operation : (K, V) -> ()) {
      for (entry in entries()) {
        operation(entry)
      }
    };

    public func filter(criterion : (K, V) -> Bool) : Map<K, V> {
      fromImperative(ImperativeMap.filter(inner, comparison : TransientCompare<K>, criterion), comparison)
    };

    // type error [M0200], cannot decide type constructor equality
    // TODO: Enable when this issue has been fixed: https://github.com/dfinity/motoko/issues/5446
    // public func map<R>(project : (K, V) -> R) : Map<K, R> {
    //   fromImperative<K, R>(ImperativeMap.map<K, V, R>(inner, project), compare)
    // };

    public func all(predicate : (K, V) -> Bool) : Bool {
      ImperativeMap.all(inner, predicate)
    };

    public func any(predicate : (K, V) -> Bool) : Bool {
      ImperativeMap.any(inner, predicate)
    };

    public func toText(keyFormat : K -> Text, valueFormat : V -> Text) : Text {
      ImperativeMap.toText(inner, keyFormat, valueFormat)
    };

    public func internalMap() : ImperativeMap.Map<K, V> {
      inner
    };

    public func internalComparison() : PersistentCompare<K> {
      comparison
    }
  };

  public persistent func fromImperative<K, V>(map : ImperativeMap.Map<K, V>, compare : PersistentCompare<K>) : Map<K, V> {
    fromIter(ImperativeMap.entries(map), compare)
  };

  public func fromPure<K, V>(pure : PureMap.Map<K, V>, compare : PersistentCompare<K>) : Map<K, V> {
    fromIter(PureMap.entries(pure), compare)
  };

  public func fromArray<K, V>(array : [(K, V)], compare : PersistentCompare<K>) : Map<K, V> {
    fromIter(array.values(), compare)
  };

  public func fromVarArray<K, V>(array : [var (K, V)], compare : PersistentCompare<K>) : Map<K, V> {
    fromIter(array.values(), compare)
  };

  public persistent func fromIter<K, V>(iter : ImperativeIter.Iter<(K, V)>, compare : PersistentCompare<K>) : Map<K, V> {
    let result = Map<K, V>(compare);
    for ((key, value) in iter) {
      result.add(key, value)
    };
    result
  };

  public func singleton<K, V>(compare : PersistentCompare<K>, key : K, value : V) : Map<K, V> {
    let result = Map<K, V>(compare);
    result.add(key, value);
    result
  };

  // `type error [M0200], cannot decide type constructor equality`
  // https://github.com/dfinity/motoko/issues/5446
  public func map<K, V1, V2>(map : Map<K, V1>, projection : (K, V1) -> V2) : Map<K, V2> {
    let result = Map<K, V2>(map.internalComparison());
    for ((key, value) in map.entries()) {
      result.add(key, projection(key, value))
    };
    result
  }
}
