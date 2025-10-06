import ImperativeIter "imperative/Iter";
import Order "Order";

module {
  public class Iter<T>(inner : ImperativeIter.Iter<T>) {
    public func forEach(operation : T -> ()) {
      ImperativeIter.forEach(inner, operation)
    };

    public func step(size : Nat) : Iter<T> {
      Iter(ImperativeIter.step(inner, size))
    };

    public func size() : Nat {
      ImperativeIter.size(inner)
    };

    public func filter(predicate : T -> Bool) : Iter<T> {
      Iter(ImperativeIter.filter(inner, predicate))
    };

    public func take(amount : Nat) : Iter<T> {
      Iter(ImperativeIter.take(inner, amount))
    };

    public func takeWhile(predicate : T -> Bool) : Iter<T> {
      Iter(ImperativeIter.takeWhile(inner, predicate))
    };

    public func drop(amount : Nat) : Iter<T> {
      Iter(ImperativeIter.drop(inner, amount))
    };

    public func dropWhile(predicate : T -> Bool) : Iter<T> {
      Iter(ImperativeIter.dropWhile(inner, predicate))
    };

    public func all(predicate : T -> Bool) : Bool {
      ImperativeIter.all(inner, predicate)
    };

    public func any(predicate : T -> Bool) : Bool {
      ImperativeIter.any(inner, predicate)
    };

    public func find(predicate : T -> Bool) : ?T {
      ImperativeIter.find(inner, predicate)
    };

    public func findIndex(predicate : T -> Bool) : ?Nat {
      ImperativeIter.findIndex(inner, predicate)
    };

    public func contains(equal : (T, T) -> Bool, value : T) : Bool {
      ImperativeIter.contains(inner, equal, value)
    };

    public func foldLeft<R>(initial : R, combine : (R, T) -> R) : R {
      ImperativeIter.foldLeft(inner, initial, combine)
    };

    public func foldRight<R>(initial : R, combine : (T, R) -> R) : R {
      ImperativeIter.foldRight(inner, initial, combine)
    };

    public func reduce(combine : (T, T) -> T) : ?T {
      ImperativeIter.reduce(inner, combine)
    };

    public func max(compare : (T, T) -> Order.Order) : ?T {
      ImperativeIter.max(inner, compare)
    };

    public func min(compare : (T, T) -> Order.Order) : ?T {
      ImperativeIter.min(inner, compare)
    };

    public func concat(other : ImperativeIter.Iter<T>) : Iter<T> {
      Iter(ImperativeIter.concat(inner, other))
    };

    public func toArray() : [T] {
      ImperativeIter.toArray(inner)
    };

    public func toVarArray() : [var T] {
      ImperativeIter.toVarArray(inner)
    };

    public func sort(compare : persistent (T, T) -> Order.Order) : Iter<T> {
      Iter(ImperativeIter.sort(inner, compare))
    };

    public func reverse() : Iter<T> {
      Iter(ImperativeIter.reverse(inner))
    };

    public func next() : ?T {
      inner.next()
    };
  };

  public func empty<T>() : Iter<T> {
    Iter(ImperativeIter.empty<T>())
  };

  public func singleton<T>(value : T) : Iter<T> {
    Iter(ImperativeIter.singleton<T>(value))
  };

  // TODO: Make object-oriented when below Motoko compiler limitation is resolved.
  // `type error [M0156], block contains expansive type definitions`.
  public func enumerate<T>(iter : ImperativeIter.Iter<T>) : Iter<(Nat, T)> {
    Iter(ImperativeIter.enumerate(iter))
  };

  // TODO: Make object-oriented when below Motoko compiler limitation is resolved.
  // `type error [M0200], cannot decide type constructor equality`
  // https://github.com/dfinity/motoko/issues/5446
  public func map<T, R>(iter : ImperativeIter.Iter<T>, projection : T -> R) : Iter<R> {
    Iter(ImperativeIter.map(iter, projection))
  };

  // TODO: Make object-oriented when below Motoko compiler limitation is resolved.
  // `type error [M0200], cannot decide type constructor equality`
  // https://github.com/dfinity/motoko/issues/5446
  public func filterMap<T, R>(iter : ImperativeIter.Iter<T>, function : T -> ?R) : Iter<R> {
    Iter(ImperativeIter.filterMap(iter, function))
  };

  public func flatten<T>(iter : ImperativeIter.Iter<Iter<T>>) : Iter<T> {
    let classical : Iter<ImperativeIter.Iter<T>> = map<Iter<T>, ImperativeIter.Iter<T>>(
      iter,
      func(iterator) {
        iterator
      }
    );
    Iter(ImperativeIter.flatten<T>(classical))
  };

  public func zip<A, B>(a : ImperativeIter.Iter<A>, b : ImperativeIter.Iter<B>) : Iter<(A, B)> {
    Iter(ImperativeIter.zip(a, b))
  };

  public func zipWith<A, B, R>(a : ImperativeIter.Iter<A>, b : ImperativeIter.Iter<B>, projection : (A, B) -> R) : Iter<R> {
    Iter(ImperativeIter.zipWith(a, b, projection))
  };

  public func scanLeft<T, R>(iter : ImperativeIter.Iter<T>, initial : R, combine : (R, T) -> R) : Iter<R> {
    Iter(ImperativeIter.scanLeft(iter, initial, combine))
  };

  public func scanRight<T, R>(iter : ImperativeIter.Iter<T>, initial : R, combine : (T, R) -> R) : Iter<R> {
    Iter(ImperativeIter.scanRight(iter, initial, combine))
  };

  public func unfold<T, S>(initial : S, step : S -> ?(T, S)) : Iter<T> {
    Iter(ImperativeIter.unfold(initial, step))
  };

  public func infinite<T>(item : T) : Iter<T> {
    Iter(ImperativeIter.infinite(item))
  };

  public func fromArray<T>(array : [T]) : Iter<T> {
    Iter(array.values())
  };

  public func fromVarArray<T>(array : [var T]) : Iter<T> {
    Iter(array.values())
  };

  public func repeat<T>(item : T, count : Nat) : Iter<T> {
    Iter(ImperativeIter.repeat(item, count))
  }
}
