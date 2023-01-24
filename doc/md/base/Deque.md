# Deque
Double-ended queue (deque) of a generic element type `T`.

The interface to deques is purely functional, not imperative, and deques are immutable values.
In particular, deque operations such as push and pop do not update their input deque but,  instead, return the
value of the modified deque, alongside any other data.
The input deque is left unchanged.

Examples of use-cases:
Queue (FIFO) by using `pushBack()` and `popFront()`.
Stack (LIFO) by using `pushFront()` and `popFront()`.

A deque is internally implemented as two lists, a head access list and a (reversed) tail access list,
that are dynamically size-balanced by splitting.

Construction: Create a new deque with the `empty<T>()` function.

Note on the costs of push and pop functions:
* Runtime: `O(1) amortized costs, `O(n)` worst case cost per single call.
* Space: `O(1) amortized costs, `O(n)` worst case cost per single call.

`n` denotes the number of elements stored in the deque.

## Type `Deque`
``` motoko no-repl
type Deque<T> = (List<T>, List<T>)
```

Double-ended queue (deque) data type.

## Function `empty`
``` motoko no-repl
func empty<T>() : Deque<T>
```

Create a new empty deque.

Example:
```motoko
import Deque "mo:base/Deque";

Deque.empty<Nat>()
```

Runtime: `O(1)`.

Space: `O(1)`.

## Function `isEmpty`
``` motoko no-repl
func isEmpty<T>(deque : Deque<T>) : Bool
```

Determine whether a deque is empty.
Returns true if `deque` is empty, otherwise `false`.

Example:
```motoko
import Deque "mo:base/Deque";

let deque = Deque.empty<Nat>();
Deque.isEmpty(deque) // => true
```

Runtime: `O(1)`.

Space: `O(1)`.

## Function `pushFront`
``` motoko no-repl
func pushFront<T>(deque : Deque<T>, element : T) : Deque<T>
```

Insert a new element on the front end of a deque.
Returns the new deque with `element` in the front followed by the elements of `deque`.

This may involve dynamic rebalancing of the two, internally used lists.

Example:
```motoko
import Deque "mo:base/Deque";

Deque.pushFront(Deque.pushFront(Deque.empty<Nat>(), 2), 1) // deque with elements [1, 2]
```

Runtime: `O(n)` worst-case, amortized to `O(1)`.

Space: `O(n)` worst-case, amortized to `O(1)`.

`n` denotes the number of elements stored in the deque.

## Function `peekFront`
``` motoko no-repl
func peekFront<T>(deque : Deque<T>) : ?T
```

Inspect the optional element on the front end of a deque.
Returns `null` if `deque` is empty. Otherwise, the front element of `deque`.

Example:
```motoko
import Deque "mo:base/Deque";

let deque = Deque.pushFront(Deque.pushFront(Deque.empty<Nat>(), 2), 1);
Deque.peekFront(deque) // => ?1
```

Runtime: `O(1)`.

Space: `O(1)`.


## Function `popFront`
``` motoko no-repl
func popFront<T>(deque : Deque<T>) : ?(T, Deque<T>)
```

Remove the element on the front end of a deque.
Returns `null` if `deque` is empty. Otherwise, it returns a pair of
the first element and a new deque that contains all the remaining elements of `deque`.

This may involve dynamic rebalancing of the two, internally used lists.

Example:
```motoko
import Deque "mo:base/Deque";
import Debug "mo:base/Debug";
let initial = Deque.pushFront(Deque.pushFront(Deque.empty<Nat>(), 2), 1);
// initial deque with elements [1, 2]
let reduced = Deque.popFront(initial);
switch reduced {
  case null {
    Debug.trap "Empty queue impossible"
  };
  case (?result) {
    let removedElement = result.0; // 1
    let reducedDeque = result.1; // deque with element [2].
  }
}
```

Runtime: `O(n)` worst-case, amortized to `O(1)`.

Space: `O(n)` worst-case, amortized to `O(1)`.

`n` denotes the number of elements stored in the deque.

## Function `pushBack`
``` motoko no-repl
func pushBack<T>(deque : Deque<T>, element : T) : Deque<T>
```

Insert a new element on the back end of a deque.
Returns the new deque with all the elements of `deque`, followed by `element` on the back.

This may involve dynamic rebalancing of the two, internally used lists.

Example:
```motoko
import Deque "mo:base/Deque";

Deque.pushBack(Deque.pushBack(Deque.empty<Nat>(), 1), 2) // deque with elements [1, 2]
```

Runtime: `O(n)` worst-case, amortized to `O(1)`.

Space: `O(n)` worst-case, amortized to `O(1)`.

`n` denotes the number of elements stored in the deque.

## Function `peekBack`
``` motoko no-repl
func peekBack<T>(deque : Deque<T>) : ?T
```

Inspect the optional element on the back end of a deque.
Returns `null` if `deque` is empty. Otherwise, the back element of `deque`.

Example:
```motoko
import Deque "mo:base/Deque";

let deque = Deque.pushBack(Deque.pushBack(Deque.empty<Nat>(), 1), 2);
Deque.peekBack(deque) // => ?2
```

Runtime: `O(1)`.

Space: `O(1)`.


## Function `popBack`
``` motoko no-repl
func popBack<T>(deque : Deque<T>) : ?(Deque<T>, T)
```

Remove the element on the back end of a deque.
Returns `null` if `deque` is empty. Otherwise, it returns a pair of
a new deque that contains the remaining elements of `deque`
and, as the second pair item, the removed back element.

This may involve dynamic rebalancing of the two, internally used lists.

Example:
```motoko
import Deque "mo:base/Deque";
import Debug "mo:base/Debug";

let initial = Deque.pushBack(Deque.pushBack(Deque.empty<Nat>(), 1), 2);
// initial deque with elements [1, 2]
let reduced = Deque.popBack(initial);
switch reduced {
  case null {
    Debug.trap "Empty queue impossible"
  };
  case (?result) {
    let reducedDeque = result.0; // deque with element [1].
    let removedElement = result.1; // 2
  }
}
```

Runtime: `O(n)` worst-case, amortized to `O(1)`.

Space: `O(n)` worst-case, amortized to `O(1)`.

`n` denotes the number of elements stored in the deque.
