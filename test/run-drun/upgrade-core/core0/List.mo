import ImperativeList "imperative/List";
import ImperativeIter "imperative/Iter";
import Iter "Iter";
import PureList "pure/List";
import Order "Order";

module {
  public persistent class List<T>() {
    let inner = ImperativeList.empty<T>();

    public func toPure() : PureList.List<T> {
      PureList.fromIter(ImperativeList.values(inner))
    };

    public func toImperative() : ImperativeList.List<T> {
      ImperativeList.clone(inner)
    };

    public func toArray() : [T] {
      ImperativeList.toArray(inner)
    };

    public func toVarArray() : [var T] {
      ImperativeList.toVarArray(inner)
    };

    public func isEmpty() : Bool {
      ImperativeList.isEmpty(inner)
    };

    public func size() : Nat {
      ImperativeList.size(inner)
    };

    public func add(element : T) {
      ImperativeList.add(inner, element)
    };

    public func get(index : Nat) : T {
      ImperativeList.at(inner, index)
    };

    public func set(index : Nat, value : T) {
      ImperativeList.put(inner, index, value)
    };

    public func removeLast() : ?T {
      ImperativeList.removeLast(inner)
    };

    public func sort(compare : persistent(T, T) -> Order.Order) {
      ImperativeList.sort(inner, compare)
    };

    public func addAll(iter : ImperativeIter.Iter<T>) {
      ImperativeList.addAll(inner, iter)
    };

    public func addRepeat(initValue : T, count : Nat) {
      ImperativeList.addRepeat(inner, initValue, count)
    };

    public func contains(equal : (T, T) -> Bool, element : T) : Bool {
      ImperativeList.contains(inner, equal, element)
    };

    public func indexOf(equal : (T, T) -> Bool, element : T) : ?Nat {
      ImperativeList.indexOf(inner, equal, element)
    };

    public func lastIndexOf(equal : (T, T) -> Bool, element : T) : ?Nat {
      ImperativeList.lastIndexOf(inner, equal, element)
    };

    public func find(predicate : T -> Bool) : ?T {
      ImperativeList.find(inner, predicate)
    };

    public func findIndex(predicate : T -> Bool) : ?Nat {
      ImperativeList.findIndex(inner, predicate)
    };

    public func findLastIndex(predicate : T -> Bool) : ?Nat {
      ImperativeList.findLastIndex(inner, predicate)
    };

    public func binarySearch(compare : (T, T) -> Order.Order, element : T) : {
      #found : Nat;
      #insertionIndex : Nat
    } {
      ImperativeList.binarySearch(inner, compare, element)
    };

    public func clone() : List<T> {
      fromImperative(inner)
    };

    public func clear() {
      ImperativeList.clear(inner)
    };

    public func equal(other : List<T>, equal : (T, T) -> Bool) : Bool {
      ImperativeList.equal<T>(inner, other.internal(), equal)
    };

    public func compare(other : List<T>, compare : (T, T) -> Order.Order) : Order.Order {
      ImperativeList.compare(inner, other.internal(), compare)
    };

    public func values() : Iter.Iter<T> {
      Iter.Iter(ImperativeList.values(inner))
    };

    public func enumerate() : Iter.Iter<(Nat, T)> {
      Iter.Iter(ImperativeList.enumerate(inner))
    };

    public func reverseValues() : Iter.Iter<T> {
      Iter.Iter(ImperativeList.reverseValues(inner))
    };

    public func reverseEnumerate() : Iter.Iter<(Nat, T)> {
      Iter.Iter(ImperativeList.reverseEnumerate(inner))
    };

    public func reverse() : List<T> {
      fromImperative(ImperativeList.reverse(inner))
    };

    public func first() : ?T {
      ImperativeList.first(inner)
    };

    public func last() : ?T {
      ImperativeList.last(inner)
    };

    public func max(compare : (T, T) -> Order.Order) : ?T {
      ImperativeList.max(inner, compare)
    };

    public func min(compare : (T, T) -> Order.Order) : ?T {
      ImperativeList.min(inner, compare)
    };

    public func forEach(operation : T -> ()) {
      for (entry in values()) {
        operation(entry)
      }
    };

    public func filter(criterion : T -> Bool) : List<T> {
      fromImperative(ImperativeList.filter(inner, criterion))
    };

    // type error [M0200], cannot decide type constructor equality
    // TODO: Enable when this issue has been fixed: https://github.com/dfinity/motoko/issues/5446
    // public func map<R>(project : T -> R) : List<R> {
    //   fromImperative<T, R>(ImperativeList.map<T, R>(inner, project))
    // };

    public func all(predicate : T -> Bool) : Bool {
      ImperativeList.all(inner, predicate)
    };

    public func any(predicate : T -> Bool) : Bool {
      ImperativeList.any(inner, predicate)
    };

    public func toText(elementFormat : T -> Text) : Text {
      ImperativeList.toText(inner, elementFormat)
    };

    public func internal() : ImperativeList.List<T> {
      inner
    }
  };

  public persistent func fromImperative<T>(list : ImperativeList.List<T>) : List<T> {
    fromIter(ImperativeList.values(list))
  };

  public func fromPure<T>(pure : PureList.List<T>) : List<T> {
    fromIter(PureList.values(PureList.reverse(pure)))
  };

  public func fromArray<T>(array : [T]) : List<T> {
    fromIter(array.values())
  };

  public func fromVarArray<T>(array : [var T]) : List<T> {
    fromIter(array.values())
  };

  public persistent func fromIter<T>(iter : ImperativeIter.Iter<T>) : List<T> {
    let result = List<T>();
    for (element in iter) {
      result.add(element)
    };
    result
  };

  public func singleton<T>(element : T) : List<T> {
    let result = List<T>();
    result.add(element);
    result
  };

  public func repeat<T>(initValue : T, size : Nat) : List<T> {
    let result = List<T>();
    result.addRepeat(initValue, size);
    result
  };

  // TODO: Make object-oriented when below Motoko compiler limitation is resolved.
  // `type error [M0200], cannot decide type constructor equality`
  // https://github.com/dfinity/motoko/issues/5446
  public func map<T, R>(list : List<T>, projection : T -> R) : List<R> {
    let result = List<R>();
    for (value in list.values()) {
      result.add(projection(value))
    };
    result
  }
}
