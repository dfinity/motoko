/// Functions for persistent, double-ended queues.

import List "List";
import P "Prelude";

module {
  type List<T> = List.List<T>;

  /// Double-ended queue
  public type Deque<T> = (List<T>, List<T>);

  /// Empty queue
  public func empty<T> () : Deque<T> { (List.nil(), List.nil()); };

  /// True when the queue is empty
  public func isEmpty<T>(q : Deque<T>) : Bool {
    switch q {
      case (f, r) { List.isNil(f) and List.isNil(r) };
    }
  };

  func check<T>(q : Deque<T>) : Deque<T> {
    switch q {
      case (null, r) { let (a,b) = List.split(List.size(r) / 2, r); (List.reverse(b), a) };
      case (f, null) { let (a,b) = List.split(List.size(f) / 2, f); (a, List.reverse(b)) };
      case q { q };
    }
  };

  /// Insert a new element on the front end of the queue
  public func pushFront<T>(q : Deque<T>, x : T) : Deque<T> {
    check (List.push(x, q.0), q.1);
  };

  /// Inspect the (optional) first element on the front end of the queue
  public func peekFront<T>(q : Deque<T>) : ?T {
    switch q {
      case (?(x, f), r) { ?x };
      case (null, ?(x, r)) { ?x };
      case _ { null };
    };
  };

  /// Remove the first element on the front end of the queue; Returns null when empty.
  public func popFront<T>(q : Deque<T>) : ?(T, Deque<T>) {
    switch q {
      case (?(x, f), r) { ?(x, check(f, r)) };
      case (null, ?(x, r)) { ?(x, check(null, r)) };
      case _ { null };
    };
  };

  /// Insert a new element on the back end of the queue
  public func pushBack<T>(q : Deque<T>, x : T) : Deque<T> {
    check (q.0, List.push(x, q.1));
  };

  /// Inspect the (optional) first element on the back end of the queue
  public func peekBack<T>(q : Deque<T>) : ?T {
    switch q {
      case (f, ?(x, r)) { ?x };
      case (?(x, r), null) { ?x };
      case _ { null };
    };
  };

  /// Remove the first element on the back end of the queue; Returns null when empty.
  public func popBack<T>(q : Deque<T>) : ?(Deque<T>, T) {
    switch q {
      case (f, ?(x, r)) { ?(check(f, r), x) };
      case (?(x, f), null) { ?(check(f, null), x) };
      case _ { null };
    };
  };
};
