import ImperativeSet "imperative/Set";
import ImperativeIter "imperative/Iter";
import Iter "Iter";
import PureSet "pure/Set";
import Order "Order";

module {
  public type PersistentCompare<T> = persistent(T, T) -> Order.Order;
  type TransientCompare<T> = (T, T) -> Order.Order;

  public persistent class Set<T>(comparison : PersistentCompare<T>) {
    let inner = ImperativeSet.empty<T>();

    public func toPure() : PureSet.Set<T> {
      PureSet.fromIter(ImperativeSet.values(inner), comparison : TransientCompare<T>)
    };

    public func toImperative() : ImperativeSet.Set<T> {
      ImperativeSet.clone(inner)
    };

    public func toArray() : [T] {
      values().toArray()
    };

    public func isEmpty() : Bool {
      ImperativeSet.isEmpty(inner)
    };

    public func size() : Nat {
      ImperativeSet.size(inner)
    };

    public func add(element : T) {
      ImperativeSet.add(inner, comparison : TransientCompare<T>, element)
    };

    public func contains(element : T) : Bool {
      ImperativeSet.contains(inner, comparison : TransientCompare<T>, element)
    };

    public func remove(element : T) {
      ImperativeSet.remove(inner, comparison : TransientCompare<T>, element)
    };

    public func clone() : Set<T> {
      fromImperative(inner, comparison)
    };

    public func clear() {
      ImperativeSet.clear(inner)
    };

    public func equal(other : Set<T>) : Bool {
      ImperativeSet.equal<T>(inner, other.internalSet(), comparison : TransientCompare<T>)
    };

    public func compare(other : Set<T>) : Order.Order {
      ImperativeSet.compare(inner, other.internalSet(), comparison : TransientCompare<T>)
    };

    public func max() : ?T {
      ImperativeSet.max(inner)
    };

    public func min() : ?T {
      ImperativeSet.min(inner)
    };

    public func values() : Iter.Iter<T> {
      Iter.Iter(ImperativeSet.values(inner))
    };

    public func reverseValues() : Iter.Iter<T> {
      Iter.Iter(ImperativeSet.reverseValues(inner))
    };

    public func forEach(operation : T -> ()) {
      for (entry in values()) {
        operation(entry)
      }
    };

    public func filter(criterion : T -> Bool) : Set<T> {
      fromImperative(ImperativeSet.filter(inner, comparison : TransientCompare<T>, criterion), comparison)
    };

    // type error [M0200], cannot decide type constructor equality
    // TODO: Enable when this issue has been fixed: https://github.com/dfinity/motoko/issues/5446
    // public func map<R>(project : T -> R, compare: PersistentCompare<R>) : Set<R> {
    //   fromImperative<T, R>(ImperativeSet.map<T, R>(inner, project), compare)
    // };

    public func all(predicate : T -> Bool) : Bool {
      ImperativeSet.all(inner, predicate)
    };

    public func any(predicate : T -> Bool) : Bool {
      ImperativeSet.any(inner, predicate)
    };

    public func toText(elementFormat : T -> Text) : Text {
      ImperativeSet.toText(inner, elementFormat)
    };

    public func internalSet() : ImperativeSet.Set<T> {
      inner
    };
  };

  public persistent func fromImperative<T>(set : ImperativeSet.Set<T>, compare : PersistentCompare<T>) : Set<T> {
    fromIter(ImperativeSet.values(set), compare)
  };

  public func fromPure<T>(pure : PureSet.Set<T>, compare : PersistentCompare<T>) : Set<T> {
    fromIter(PureSet.values(pure), compare)
  };

  public func fromArray<T>(array : [T], compare : PersistentCompare<T>) : Set<T> {
    fromIter(array.values(), compare)
  };

  public persistent func fromIter<T>(iter : ImperativeIter.Iter<T>, compare : PersistentCompare<T>) : Set<T> {
    let result = Set<T>(compare);
    for (element in iter) {
      result.add(element)
    };
    result
  };

  public func singleton<T>(compare : PersistentCompare<T>, element : T) : Set<T> {
    let result = Set<T>(compare);
    result.add(element);
    result
  };

  // `type error [M0200], cannot decide type constructor equality`
  // https://github.com/dfinity/motoko/issues/5446
  public func map<T, R>(set : Set<T>, projection : T -> R, compare: PersistentCompare<R>) : Set<R> {
    let result = Set<R>(compare);
    for (value in set.values()) {
      result.add(projection(value))
    };
    result
  }
}
