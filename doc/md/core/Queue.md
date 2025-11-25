# core/Queue
A mutable double-ended queue of elements.
The queue has two ends, front and back.
Elements can be added and removed at the two ends.

This can be used for different use cases, such as:
* Queue (FIFO) by using `pushBack()` and `popFront()`
* Stack (LIFO) by using `pushFront()` and `popFront()`.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let orders = Queue.empty<Text>();
  Queue.pushBack(orders, "Motoko");
  Queue.pushBack(orders, "Mops");
  Queue.pushBack(orders, "IC");
  assert Queue.popFront(orders) == ?"Motoko";
  assert Queue.popFront(orders) == ?"Mops";
  assert Queue.popFront(orders) == ?"IC";
  assert Queue.popFront(orders) == null;
}
```

The internal implementation is a doubly-linked list.

Performance:
* Runtime: `O(1)` for push, pop, and peek operations.
* Space: `O(n)`.
`n` denotes the number of elements stored in the queue.

## Type `Queue`
``` motoko no-repl
type Queue<T> = Types.Queue.Queue<T>
```


## Function `toPure`
``` motoko no-repl
func toPure<T>(queue : Queue<T>) : PureQueue.Queue<T>
```

Converts a mutable queue to an immutable, purely functional queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  let pureQueue = Queue.toPure<Nat>(queue);
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `fromPure`
``` motoko no-repl
func fromPure<T>(pureQueue : PureQueue.Queue<T>) : Queue<T>
```

Converts an immutable, purely functional queue to a mutable queue.

Example:
```motoko
import Queue "mo:core/Queue";
import PureQueue "mo:core/pure/Queue";

persistent actor {
  let pureQueue = PureQueue.fromIter<Nat>([1, 2, 3].values());
  let queue = Queue.fromPure<Nat>(pureQueue);
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `empty`
``` motoko no-repl
func empty<T>() : Queue<T>
```

Create a new empty mutable double-ended queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.empty<Text>();
  assert Queue.size(queue) == 0;
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `singleton`
``` motoko no-repl
func singleton<T>(element : T) : Queue<T>
```

Creates a new queue with a single element.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.singleton<Nat>(123);
  assert Queue.size(queue) == 1;
}
```

Runtime: O(1)
Space: O(1)

## Function `clear`
``` motoko no-repl
func clear<T>(queue : Queue<T>)
```

Removes all elements from the queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  Queue.clear(queue);
  assert Queue.isEmpty(queue);
}
```

Runtime: O(1)
Space: O(1)

## Function `clone`
``` motoko no-repl
func clone<T>(queue : Queue<T>) : Queue<T>
```

Creates a deep copy of the queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let original = Queue.fromIter<Nat>([1, 2, 3].values());
  let copy = Queue.clone(original);
  Queue.clear(original);
  assert Queue.size(original) == 0;
  assert Queue.size(copy) == 3;
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `size`
``` motoko no-repl
func size<T>(queue : Queue<T>) : Nat
```

Returns the number of elements in the queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Text>(["A", "B", "C"].values());
  assert Queue.size(queue) == 3;
}
```

Runtime: O(1)
Space: O(1)

## Function `isEmpty`
``` motoko no-repl
func isEmpty<T>(queue : Queue<T>) : Bool
```

Returns `true` if the queue contains no elements.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.empty<Nat>();
  assert Queue.isEmpty(queue);
}
```

Runtime: O(1)
Space: O(1)

## Function `contains`
``` motoko no-repl
func contains<T>(queue : Queue<T>, equal : (T, T) -> Bool, element : T) : Bool
```

Checks if an element exists in the queue using the provided equality function.

Example:
```motoko
import Queue "mo:core/Queue";
import Nat "mo:core/Nat";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.contains(queue, Nat.equal, 2);
}
```

Runtime: O(n)
Space: O(1)
`n` denotes the number of elements stored in the queue.

## Function `peekFront`
``` motoko no-repl
func peekFront<T>(queue : Queue<T>) : ?T
```

Returns the first element in the queue without removing it.
Returns null if the queue is empty.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.peekFront(queue) == ?1;
}
```

Runtime: O(1)
Space: O(1)

## Function `peekBack`
``` motoko no-repl
func peekBack<T>(queue : Queue<T>) : ?T
```

Returns the last element in the queue without removing it.
Returns null if the queue is empty.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.peekBack(queue) == ?3;
}
```

Runtime: O(1)
Space: O(1)

## Function `pushFront`
``` motoko no-repl
func pushFront<T>(queue : Queue<T>, element : T)
```

Adds an element to the front of the queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.empty<Nat>();
  Queue.pushFront(queue, 1);
  assert Queue.peekFront(queue) == ?1;
}
```

Runtime: O(1)
Space: O(1)

## Function `pushBack`
``` motoko no-repl
func pushBack<T>(queue : Queue<T>, element : T)
```

Adds an element to the back of the queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.empty<Nat>();
  Queue.pushBack(queue, 1);
  assert Queue.peekBack(queue) == ?1;
}
```

Runtime: O(1)
Space: O(1)

## Function `popFront`
``` motoko no-repl
func popFront<T>(queue : Queue<T>) : ?T
```

Removes and returns the first element in the queue.
Returns null if the queue is empty.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.popFront(queue) == ?1;
  assert Queue.size(queue) == 2;
}
```

Runtime: O(1)
Space: O(1)

## Function `popBack`
``` motoko no-repl
func popBack<T>(queue : Queue<T>) : ?T
```

Removes and returns the last element in the queue.
Returns null if the queue is empty.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.popBack(queue) == ?3;
  assert Queue.size(queue) == 2;
}
```

Runtime: O(1)
Space: O(1)

## Function `fromIter`
``` motoko no-repl
func fromIter<T>(iter : Iter.Iter<T>) : Queue<T>
```

Creates a new queue from an iterator.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Text>(["A", "B", "C"].values());
  assert Queue.size(queue) == 3;
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `fromArray`
``` motoko no-repl
func fromArray<T>(array : [T]) : Queue<T>
```

Creates a new queue from an array.
Elements appear in the same order as in the array.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromArray<Text>(["A", "B", "C"]);
  assert Queue.size(queue) == 3;
  assert Queue.peekFront(queue) == ?"A";
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the array.

## Function `toArray`
``` motoko no-repl
func toArray<T>(queue : Queue<T>) : [T]
```

Creates a new immutable array containing all elements from the queue.
Elements appear in the same order as in the queue (front to back).

Example:
```motoko
import Queue "mo:core/Queue";
import Array "mo:core/Array";

persistent actor {
  let queue = Queue.fromArray<Text>(["A", "B", "C"]);
  let array = Queue.toArray(queue);
  assert array == ["A", "B", "C"];
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `values`
``` motoko no-repl
func values<T>(queue : Queue<T>) : Iter.Iter<T>
```

Returns an iterator over the elements in the queue.
Iterates from front to back.

Example:
```motoko
import Queue "mo:core/Queue";
persistent actor {
  let queue = Queue.fromIter<Text>(["A", "B", "C"].values());
  transient let iter = Queue.values(queue);
  assert iter.next() == ?"A";
  assert iter.next() == ?"B";
  assert iter.next() == ?"C";
  assert iter.next() == null;
}
```

Runtime: O(1) for iterator creation, O(n) for full iteration
Space: O(1)

## Function `all`
``` motoko no-repl
func all<T>(queue : Queue<T>, predicate : T -> Bool) : Bool
```

Tests whether all elements in the queue satisfy the given predicate.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([2, 4, 6].values());
  assert Queue.all<Nat>(queue, func(x) { x % 2 == 0 });
}
```

Runtime: O(n)
Space: O(1)

## Function `any`
``` motoko no-repl
func any<T>(queue : Queue<T>, predicate : T -> Bool) : Bool
```

Tests whether any element in the queue satisfies the given predicate.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.any<Nat>(queue, func (x) { x > 2 });
}
```

Runtime: O(n)
Space: O(1)
`n` denotes the number of elements stored in the queue.

## Function `forEach`
``` motoko no-repl
func forEach<T>(queue : Queue<T>, operation : T -> ())
```

Applies the given operation to all elements in the queue.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  var sum = 0;
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  Queue.forEach<Nat>(queue, func(x) { sum += x });
  assert sum == 6;
}
```

Runtime: O(n)
Space: O(1)
`n` denotes the number of elements stored in the queue.

## Function `map`
``` motoko no-repl
func map<T, U>(queue : Queue<T>, project : T -> U) : Queue<U>
```

Creates a new queue by applying the given function to all elements.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  let doubled = Queue.map<Nat, Nat>(queue, func(x) { x * 2 });
  assert Queue.peekFront(doubled) == ?2;
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `filter`
``` motoko no-repl
func filter<T>(queue : Queue<T>, criterion : T -> Bool) : Queue<T>
```

Creates a new queue containing only elements that satisfy the given predicate.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3, 4].values());
  let evens = Queue.filter<Nat>(queue, func(x) { x % 2 == 0 });
  assert Queue.size(evens) == 2;
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `filterMap`
``` motoko no-repl
func filterMap<T, U>(queue : Queue<T>, project : T -> ?U) : Queue<U>
```

Creates a new queue by applying the given function to all elements
and keeping only the non-null results.

Example:
```motoko
import Queue "mo:core/Queue";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3, 4].values());
  let evenDoubled = Queue.filterMap<Nat, Nat>(
    queue,
    func(x) {
      if (x % 2 == 0) { ?(x * 2) } else  { null }
    }
  );
  assert Queue.size(evenDoubled) == 2;
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `equal`
``` motoko no-repl
func equal<T>(queue1 : Queue<T>, queue2 : Queue<T>, equal : (T, T) -> Bool) : Bool
```

Compares two queues for equality using the provided equality function.

Example:
```motoko
import Queue "mo:core/Queue";
import Nat "mo:core/Nat";

persistent actor {
  let queue1 = Queue.fromIter<Nat>([1, 2, 3].values());
  let queue2 = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.equal(queue1, queue2, Nat.equal);
}
```

Runtime: O(n)
Space: O(1)
`n` denotes the number of elements stored in the queue.

## Function `toText`
``` motoko no-repl
func toText<T>(queue : Queue<T>, format : T -> Text) : Text
```

Converts a queue to its string representation using the provided element formatter.

Example:
```motoko
import Queue "mo:core/Queue";
import Nat "mo:core/Nat";

persistent actor {
  let queue = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.toText(queue, Nat.toText) == "Queue[1, 2, 3]";
}
```

Runtime: O(n)
Space: O(n)
`n` denotes the number of elements stored in the queue.

## Function `compare`
``` motoko no-repl
func compare<T>(queue1 : Queue<T>, queue2 : Queue<T>, compare : (T, T) -> Order.Order) : Order.Order
```

Compares two queues using the provided comparison function.
Returns #less, #equal, or #greater.

Example:
```motoko
import Queue "mo:core/Queue";
import Nat "mo:core/Nat";

persistent actor {
  let queue1 = Queue.fromIter<Nat>([1, 2].values());
  let queue2 = Queue.fromIter<Nat>([1, 2, 3].values());
  assert Queue.compare(queue1, queue2, Nat.compare) == #less;
}
```

Runtime: O(n)
Space: O(1)
`n` denotes the number of elements stored in the queue.
