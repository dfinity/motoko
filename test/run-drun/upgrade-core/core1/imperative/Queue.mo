/// A mutable double-ended queue of elements.
/// The queue has two ends, front and back.
/// Elements can be added and removed at the two ends.
///
/// This can be used for different use cases, such as:
/// * Queue (FIFO) by using `pushBack()` and `popFront()`
/// * Stack (LIFO) by using `pushFront()` and `popFront()`.
///
/// Example:
/// ```motoko
/// import Queue "mo:core/Queue";
///
/// persistent actor {
///   let orders = Queue.empty<Text>();
///   Queue.pushBack(orders, "Motoko");
///   Queue.pushBack(orders, "Mops");
///   Queue.pushBack(orders, "IC");
///   assert Queue.popFront(orders) == ?"Motoko";
///   assert Queue.popFront(orders) == ?"Mops";
///   assert Queue.popFront(orders) == ?"IC";
///   assert Queue.popFront(orders) == null;
/// }
/// ```
///
/// The internal implementation is a doubly-linked list.
///
/// Performance:
/// * Runtime: `O(1)` for push, pop, and peek operations.
/// * Space: `O(n)`.
/// `n` denotes the number of elements stored in the queue.

import PureQueue "../pure/Queue";
import Iter "Iter";
import Order "../Order";
import Types "../Types";
import Array "../Array";
import Prim "mo:⛔";

module {
  public type Queue<T> = Types.Queue.Queue<T>;

  type Node<T> = Types.Queue.Node<T>;

  /// Converts a mutable queue to an immutable, purely functional queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   let pureQueue = Queue.toPure<Nat>(queue);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func toPure<T>(queue : Queue<T>) : PureQueue.Queue<T> {
    let pureQueue = PureQueue.empty<T>();
    let iter = values(queue);
    var current = pureQueue;
    loop {
      switch (iter.next()) {
        case null { return current };
        case (?val) { current := PureQueue.pushBack(current, val) }
      }
    }
  };

  /// Converts an immutable, purely functional queue to a mutable queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// import PureQueue "mo:core/pure/Queue";
  ///
  /// persistent actor {
  ///   let pureQueue = PureQueue.fromIter<Nat>([1, 2, 3].values());
  ///   let queue = Queue.fromPure<Nat>(pureQueue);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func fromPure<T>(pureQueue : PureQueue.Queue<T>) : Queue<T> {
    let queue = empty<T>();
    let iter = PureQueue.values(pureQueue);
    loop {
      switch (iter.next()) {
        case null { return queue };
        case (?val) { pushBack(queue, val) }
      }
    }
  };

  /// Create a new empty mutable double-ended queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.empty<Text>();
  ///   assert Queue.size(queue) == 0;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func empty<T>() : Queue<T> {
    { var front = null; var back = null; var size = 0 }
  };

  /// Creates a new queue with a single element.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.singleton<Nat>(123);
  ///   assert Queue.size(queue) == 1;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func singleton<T>(element : T) : Queue<T> {
    let queue = empty<T>();
    pushBack(queue, element);
    queue
  };

  /// Removes all elements from the queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   Queue.clear(queue);
  ///   assert Queue.isEmpty(queue);
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func clear<T>(queue : Queue<T>) {
    queue.front := null;
    queue.back := null;
    queue.size := 0
  };

  /// Creates a deep copy of the queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let original = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   let copy = Queue.clone(original);
  ///   Queue.clear(original);
  ///   assert Queue.size(original) == 0;
  ///   assert Queue.size(copy) == 3;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func clone<T>(queue : Queue<T>) : Queue<T> {
    let copy = empty<T>();
    for (element in values(queue)) {
      pushBack(copy, element)
    };
    copy
  };

  /// Returns the number of elements in the queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Text>(["A", "B", "C"].values());
  ///   assert Queue.size(queue) == 3;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func size<T>(queue : Queue<T>) : Nat {
    queue.size
  };

  /// Returns `true` if the queue contains no elements.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.empty<Nat>();
  ///   assert Queue.isEmpty(queue);
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func isEmpty<T>(queue : Queue<T>) : Bool {
    queue.size == 0
  };

  /// Checks if an element exists in the queue using the provided equality function.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.contains(queue, Nat.equal, 2);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// `n` denotes the number of elements stored in the queue.
  public func contains<T>(queue : Queue<T>, equal : (T, T) -> Bool, element : T) : Bool {
    for (existing in values(queue)) {
      if (equal(existing, element)) {
        return true
      }
    };
    false
  };

  /// Returns the first element in the queue without removing it.
  /// Returns null if the queue is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.peekFront(queue) == ?1;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func peekFront<T>(queue : Queue<T>) : ?T {
    switch (queue.front) {
      case null null;
      case (?node) ?node.value
    }
  };

  /// Returns the last element in the queue without removing it.
  /// Returns null if the queue is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.peekBack(queue) == ?3;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func peekBack<T>(queue : Queue<T>) : ?T {
    switch (queue.back) {
      case null null;
      case (?node) ?node.value
    }
  };

  /// Adds an element to the front of the queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.empty<Nat>();
  ///   Queue.pushFront(queue, 1);
  ///   assert Queue.peekFront(queue) == ?1;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func pushFront<T>(queue : Queue<T>, element : T) {
    let node : Node<T> = {
      value = element;
      var next = queue.front;
      var previous = null
    };
    switch (queue.front) {
      case null {};
      case (?first) first.previous := ?node
    };
    queue.front := ?node;
    switch (queue.back) {
      case null queue.back := ?node;
      case (?_) {}
    };
    queue.size += 1
  };

  /// Adds an element to the back of the queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.empty<Nat>();
  ///   Queue.pushBack(queue, 1);
  ///   assert Queue.peekBack(queue) == ?1;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func pushBack<T>(queue : Queue<T>, element : T) {
    let node : Node<T> = {
      value = element;
      var next = null;
      var previous = queue.back
    };
    switch (queue.back) {
      case null {};
      case (?last) last.next := ?node
    };
    queue.back := ?node;
    switch (queue.front) {
      case null queue.front := ?node;
      case (?_) {}
    };
    queue.size += 1
  };

  /// Removes and returns the first element in the queue.
  /// Returns null if the queue is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.popFront(queue) == ?1;
  ///   assert Queue.size(queue) == 2;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func popFront<T>(queue : Queue<T>) : ?T {
    switch (queue.front) {
      case null null;
      case (?first) {
        queue.front := first.next;
        switch (queue.front) {
          case null { queue.back := null };
          case (?newFirst) { newFirst.previous := null }
        };
        queue.size -= 1;
        ?first.value
      }
    }
  };

  /// Removes and returns the last element in the queue.
  /// Returns null if the queue is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.popBack(queue) == ?3;
  ///   assert Queue.size(queue) == 2;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func popBack<T>(queue : Queue<T>) : ?T {
    switch (queue.back) {
      case null null;
      case (?last) {
        queue.back := last.previous;
        switch (queue.back) {
          case null { queue.front := null };
          case (?newLast) { newLast.next := null }
        };
        queue.size -= 1;
        ?last.value
      }
    }
  };

  /// Creates a new queue from an iterator.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Text>(["A", "B", "C"].values());
  ///   assert Queue.size(queue) == 3;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func fromIter<T>(iter : Iter.Iter<T>) : Queue<T> {
    let queue = empty<T>();
    for (element in iter) {
      pushBack(queue, element)
    };
    queue
  };

  /// Creates a new queue from an array.
  /// Elements appear in the same order as in the array.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromArray<Text>(["A", "B", "C"]);
  ///   assert Queue.size(queue) == 3;
  ///   assert Queue.peekFront(queue) == ?"A";
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the array.
  public func fromArray<T>(array : [T]) : Queue<T> {
    let queue = empty<T>();
    for (element in array.vals()) {
      pushBack(queue, element)
    };
    queue
  };

  /// Creates a new immutable array containing all elements from the queue.
  /// Elements appear in the same order as in the queue (front to back).
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// import Array "mo:core/Array";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromArray<Text>(["A", "B", "C"]);
  ///   let array = Queue.toArray(queue);
  ///   assert array == ["A", "B", "C"];
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func toArray<T>(queue : Queue<T>) : [T] {
    let iter = values(queue);
    Array.tabulate<T>(
      queue.size,
      func(i) {
        switch (iter.next()) {
          case null { Prim.trap("Queue.toArray(): unexpected end of iterator") };
          case (?value) { value }
        }
      }
    )
  };

  /// Returns an iterator over the elements in the queue.
  /// Iterates from front to back.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// persistent actor {
  ///   let queue = Queue.fromIter<Text>(["A", "B", "C"].values());
  ///   transient let iter = Queue.values(queue);
  ///   assert iter.next() == ?"A";
  ///   assert iter.next() == ?"B";
  ///   assert iter.next() == ?"C";
  ///   assert iter.next() == null;
  /// }
  /// ```
  ///
  /// Runtime: O(1) for iterator creation, O(n) for full iteration
  /// Space: O(1)
  public func values<T>(queue : Queue<T>) : Iter.Iter<T> {
    object {
      var current = queue.front;

      public func next() : ?T {
        switch (current) {
          case null null;
          case (?node) {
            current := node.next;
            ?node.value
          }
        }
      }
    }
  };

  /// Returns an iterator over the elements in the queue, in reverse order.
  /// Iterates from back to front.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// persistent actor {
  ///   let queue = Queue.fromIter<Text>(["A", "B", "C"].values());
  ///   transient let iter = Queue.reverseValues(queue);
  ///   assert iter.next() == ?"C";
  ///   assert iter.next() == ?"B";
  ///   assert iter.next() == ?"A";
  ///   assert iter.next() == null;
  /// }
  /// ```
  ///
  /// Runtime: O(1) for iterator creation, O(n) for full iteration
  /// Space: O(1)
  public func reverseValues<T>(queue : Queue<T>) : Iter.Iter<T> {
    object {
      var current = queue.back;

      public func next() : ?T {
        switch (current) {
          case null null;
          case (?node) {
            current := node.previous;
            ?node.value
          }
        }
      }
    }
  };

  /// Tests whether all elements in the queue satisfy the given predicate.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([2, 4, 6].values());
  ///   assert Queue.all<Nat>(queue, func(x) { x % 2 == 0 });
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  public func all<T>(queue : Queue<T>, predicate : T -> Bool) : Bool {
    for (element in values(queue)) {
      if (not predicate(element)) {
        return false
      }
    };
    true
  };

  /// Tests whether any element in the queue satisfies the given predicate.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.any<Nat>(queue, func (x) { x > 2 });
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// `n` denotes the number of elements stored in the queue.
  public func any<T>(queue : Queue<T>, predicate : T -> Bool) : Bool {
    for (element in values(queue)) {
      if (predicate(element)) {
        return true
      }
    };
    false
  };

  /// Applies the given operation to all elements in the queue.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   var sum = 0;
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   Queue.forEach<Nat>(queue, func(x) { sum += x });
  ///   assert sum == 6;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// `n` denotes the number of elements stored in the queue.
  public func forEach<T>(queue : Queue<T>, operation : T -> ()) {
    for (element in values(queue)) {
      operation(element)
    }
  };

  /// Creates a new queue by applying the given function to all elements.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   let doubled = Queue.map<Nat, Nat>(queue, func(x) { x * 2 });
  ///   assert Queue.peekFront(doubled) == ?2;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func map<T, U>(queue : Queue<T>, project : T -> U) : Queue<U> {
    let result = empty<U>();
    for (element in values(queue)) {
      pushBack(result, project(element))
    };
    result
  };

  /// Creates a new queue containing only elements that satisfy the given predicate.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3, 4].values());
  ///   let evens = Queue.filter<Nat>(queue, func(x) { x % 2 == 0 });
  ///   assert Queue.size(evens) == 2;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func filter<T>(queue : Queue<T>, criterion : T -> Bool) : Queue<T> {
    let result = empty<T>();
    for (element in values(queue)) {
      if (criterion(element)) {
        pushBack(result, element)
      }
    };
    result
  };

  /// Creates a new queue by applying the given function to all elements
  /// and keeping only the non-null results.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3, 4].values());
  ///   let evenDoubled = Queue.filterMap<Nat, Nat>(
  ///     queue,
  ///     func(x) {
  ///       if (x % 2 == 0) { ?(x * 2) } else  { null }
  ///     }
  ///   );
  ///   assert Queue.size(evenDoubled) == 2;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func filterMap<T, U>(queue : Queue<T>, project : T -> ?U) : Queue<U> {
    let result = empty<U>();
    for (element in values(queue)) {
      switch (project(element)) {
        case null {};
        case (?newElement) pushBack(result, newElement)
      }
    };
    result
  };

  /// Compares two queues for equality using the provided equality function.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let queue1 = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   let queue2 = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.equal(queue1, queue2, Nat.equal);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// `n` denotes the number of elements stored in the queue.
  public func equal<T>(queue1 : Queue<T>, queue2 : Queue<T>, equal : (T, T) -> Bool) : Bool {
    if (size(queue1) != size(queue2)) {
      return false
    };
    let iterator1 = values(queue1);
    let iterator2 = values(queue2);
    loop {
      let element1 = iterator1.next();
      let element2 = iterator2.next();
      switch (element1, element2) {
        case (null, null) {
          return true
        };
        case (?element1, ?element2) {
          if (not equal(element1, element2)) {
            return false
          }
        };
        case _ { return false }
      }
    }
  };

  /// Converts a queue to its string representation using the provided element formatter.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.toText(queue, Nat.toText) == "Queue[1, 2, 3]";
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// `n` denotes the number of elements stored in the queue.
  public func toText<T>(queue : Queue<T>, format : T -> Text) : Text {
    var text = "Queue[";
    var sep = "";
    for (element in values(queue)) {
      text #= sep # format(element);
      sep := ", "
    };
    text #= "]";
    text
  };

  /// Compares two queues using the provided comparison function.
  /// Returns #less, #equal, or #greater.
  ///
  /// Example:
  /// ```motoko
  /// import Queue "mo:core/Queue";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let queue1 = Queue.fromIter<Nat>([1, 2].values());
  ///   let queue2 = Queue.fromIter<Nat>([1, 2, 3].values());
  ///   assert Queue.compare(queue1, queue2, Nat.compare) == #less;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// `n` denotes the number of elements stored in the queue.
  public func compare<T>(queue1 : Queue<T>, queue2 : Queue<T>, compare : (T, T) -> Order.Order) : Order.Order {
    let iterator1 = values(queue1);
    let iterator2 = values(queue2);
    loop {
      switch (iterator1.next(), iterator2.next()) {
        case (null, null) return #equal;
        case (null, _) return #less;
        case (_, null) return #greater;
        case (?element1, ?element2) {
          let comparison = compare(element1, element2);
          if (comparison != #equal) {
            return comparison
          }
        }
      }
    }
  }
}
