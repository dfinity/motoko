import ImperativeQueue "imperative/Queue";
import ImperativeIter "imperative/Iter";
import Iter "Iter";
import PureQueue "pure/Queue";
import Order "Order";
import Array "Array";

module {
  public persistent class Queue<T>() {
    let inner = ImperativeQueue.empty<T>();

    public func toPure() : PureQueue.Queue<T> {
      PureQueue.fromIter(ImperativeQueue.values(inner))
    };

    public func toImperative() : ImperativeQueue.Queue<T> {
      ImperativeQueue.clone(inner)
    };

    public func toArray() : [T] {
      ImperativeQueue.toArray(inner)
    };

    public func toVarArray() : [var T] {
      Array.toVarArray(toArray())
    };

    public func isEmpty() : Bool {
      ImperativeQueue.isEmpty(inner)
    };

    public func size() : Nat {
      ImperativeQueue.size(inner)
    };

    public func pushFront(element : T) {
      ImperativeQueue.pushFront(inner, element)
    };

    public func pushBack(element : T) {
      ImperativeQueue.pushBack(inner, element)
    };

    public func popFront() : ?T {
      ImperativeQueue.popFront(inner)
    };

    public func popBack() : ?T {
      ImperativeQueue.popBack(inner)
    };

    public func peekFront() : ?T {
      ImperativeQueue.peekFront(inner)
    };

    public func peekBack() : ?T {
      ImperativeQueue.peekBack(inner)
    };

    public func contains(equal : (T, T) -> Bool, element : T) : Bool {
      ImperativeQueue.contains(inner, equal, element)
    };

    public func clone() : Queue<T> {
      fromImperative(inner)
    };

    public func clear() {
      ImperativeQueue.clear(inner)
    };

    public func values() : Iter.Iter<T> {
      Iter.Iter(ImperativeQueue.values(inner))
    };

    public func reverseValues() : Iter.Iter<T> {
      Iter.Iter(ImperativeQueue.reverseValues(inner))
    };

    public func equal(other : Queue<T>, equal : (T, T) -> Bool) : Bool {
      ImperativeQueue.equal<T>(inner, other.internal(), equal)
    };

    public func compare(other : Queue<T>, compare : (T, T) -> Order.Order) : Order.Order {
      ImperativeQueue.compare(inner, other.internal(), compare)
    };

    public func internal() : ImperativeQueue.Queue<T> {
      inner
    }
  };

  public persistent func fromImperative<T>(queue : ImperativeQueue.Queue<T>) : Queue<T> {
    fromIter(ImperativeQueue.values(queue))
  };

  public func fromPure<T>(pure : PureQueue.Queue<T>) : Queue<T> {
    fromIter(PureQueue.values(pure))
  };

  public func fromArray<T>(array : [T]) : Queue<T> {
    fromIter(array.values())
  };

  public func fromVarArray<T>(array : [var T]) : Queue<T> {
    fromIter(array.values())
  };

  public persistent func fromIter<T>(iter : ImperativeIter.Iter<T>) : Queue<T> {
    let result = Queue<T>();
    for (element in iter) {
      result.pushBack(element)
    };
    result
  };

  public func singleton<T>(element : T) : Queue<T> {
    let result = Queue<T>();
    result.pushBack(element);
    result
  }
}
