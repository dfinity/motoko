# OrderedSet
Stable ordered set implemented as a red-black tree.

A red-black tree is a balanced binary search tree ordered by the elements.

The tree data structure internally colors each of its nodes either red or black,
and uses this information to balance the tree during the modifying operations.

Performance:
* Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
* Space: `O(n)` for storing the entire tree.
`n` denotes the number of elements (i.e. nodes) stored in the tree.

Credits:

The core of this implementation is derived from:

* Ken Friis Larsen's [RedBlackMap.sml](https://github.com/kfl/mosml/blob/master/src/mosmllib/Redblackmap.sml), which itself is based on:
* Stefan Kahrs, "Red-black trees with types", Journal of Functional Programming, 11(4): 425-432 (2001), [version 1 in web appendix](http://www.cs.ukc.ac.uk/people/staff/smk/redblack/rb.html).

## Type `Set`
``` motoko no-repl
type Set<T> = { size : Nat; root : Tree<T> }
```

Ordered collection of unique elements of the generic type `T`.
If type `T` is stable then `Set<T>` is also stable.
To ensure that property the `Set<T>` does not have any methods,
instead they are gathered in the functor-like class `Operations` (see example there).

## Class `Operations<T>`

``` motoko no-repl
class Operations<T>(compare : (T, T) -> O.Order)
```

Class that captures element type `T` along with its ordering function `compare`
and provide all operations to work with a set of type `Set<T>`.

An instance object should be created once as a canister field to ensure
that the same ordering function is used for every operation.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";

actor {
  let natSet = Set.Make<Nat>(Nat.compare); // : Operations<Nat>
  stable var usedIds : Set.Set<Nat> = natSet.empty();
  
  public func createId(id : Nat) : async () {
    usedIds := natSet.put(usedIds, id);
  };
  
  public func idIsUsed(id: Nat) : async Bool {
     natSet.contains(usedIds, id)
  }
}
```

### Function `fromIter`
``` motoko no-repl
func fromIter(i : I.Iter<T>) : Set<T>
```

Returns a new Set, containing all entries given by the iterator `i`.
If there are multiple identical entries only one is taken.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show(Iter.toArray(natSet.vals(set))));
// [0, 1, 2]
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.


### Function `put`
``` motoko no-repl
func put(s : Set<T>, value : T) : Set<T>
```

Insert the value `value` into the set `s`. Has no effect if `value` is already
present in the set. Returns a modified set.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
var set = natSet.empty();

set := natSet.put(set, 0);
set := natSet.put(set, 2);
set := natSet.put(set, 1);

Debug.print(debug_show(Iter.toArray(natSet.vals(set))));
// [0, 1, 2]
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned set shares with the `s` most of the tree nodes. 
Garbage collecting one of sets (e.g. after an assignment `m := natSet.delete(m, k)`)
causes collecting `O(log(n))` nodes.


### Function `delete`
``` motoko no-repl
func delete(s : Set<T>, value : T) : Set<T>
```

Deletes the value `value` from the set `s`. Has no effect if `value` is not
present in the set. Returns modified set.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show(Iter.toArray(natSet.vals(natSet.delete(set, 1)))));
Debug.print(debug_show(Iter.toArray(natSet.vals(natSet.delete(set, 42)))));
// [0, 2]
// [0, 1, 2]
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned set shares with the `s` most of the tree nodes. 
Garbage collecting one of sets (e.g. after an assignment `m := natSet.delete(m, k)`)
causes collecting `O(log(n))` nodes.


### Function `contains`
``` motoko no-repl
func contains(s : Set<T>, value : T) : Bool
```

Test if the set 's' contains a given element.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show natSet.contains(set, 1)); // => true
Debug.print(debug_show natSet.contains(set, 42)); // => false
```

Runtime: `O(log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.


### Function `max`
``` motoko no-repl
func max(s : Set<T>) : ?T
```

Get a maximal element of the set `s` if it is not empty, otherwise returns `null`

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let s1 = natSet.fromIter(Iter.fromArray([0, 2, 1]));
let s2 = natSet.empty();

Debug.print(debug_show(natSet.max(s1))); // => ?2
Debug.print(debug_show(natSet.max(s2))); // => null
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of elements in the set


### Function `min`
``` motoko no-repl
func min(s : Set<T>) : ?T
```

Get a minimal element of the set `s` if it is not empty, otherwise returns `null`

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let s1 = natSet.fromIter(Iter.fromArray([0, 2, 1]));
let s2 = natSet.empty();

Debug.print(debug_show(natSet.min(s1))); // => ?0
Debug.print(debug_show(natSet.min(s2))); // => null
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of elements in the set


### Function `union`
``` motoko no-repl
func union(s1 : Set<T>, s2 : Set<T>) : Set<T>
```

[Set union](https://en.wikipedia.org/wiki/Union_(set_theory)) operation.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set1 = natSet.fromIter(Iter.fromArray([0, 1, 2]));
let set2 = natSet.fromIter(Iter.fromArray([2, 3, 4]));

Debug.print(debug_show Iter.toArray(natSet.vals(natSet.union(set1, set2))));
// [0, 1, 2, 3, 4]
```

Runtime: `O(m * log(n))`.
Space: `O(m)`, retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements in the sets, and `m <= n`.

Note: Creates `O(m * log(n))` temporary objects that will be collected as garbage.


### Function `intersect`
``` motoko no-repl
func intersect(s1 : Set<T>, s2 : Set<T>) : Set<T>
```

[Set intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory)) operation.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set1 = natSet.fromIter(Iter.fromArray([0, 1, 2]));
let set2 = natSet.fromIter(Iter.fromArray([1, 2, 3]));

Debug.print(debug_show Iter.toArray(natSet.vals(natSet.intersect(set1, set2))));
// [1, 2]
```

Runtime: `O(m * log(n))`.
Space: `O(m)`, retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements in the sets, and `m <= n`.

Note: Creates `O(m)` temporary objects that will be collected as garbage.


### Function `diff`
``` motoko no-repl
func diff(s1 : Set<T>, s2 : Set<T>) : Set<T>
```

[Set difference](https://en.wikipedia.org/wiki/Difference_(set_theory)).

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set1 = natSet.fromIter(Iter.fromArray([0, 1, 2]));
let set2 = natSet.fromIter(Iter.fromArray([1, 2, 3]));

Debug.print(debug_show Iter.toArray(natSet.vals(natSet.diff(set1, set2))));
// [0]
```

Runtime: `O(m * log(n))`.
Space: `O(m)`, retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements in the sets, and `m <= n`.

Note: Creates `O(m * log(n))` temporary objects that will be collected as garbage.


### Function `map`
``` motoko no-repl
func map<T1>(s : Set<T1>, f : T1 -> T) : Set<T>
```

Creates a new `Set` by applying `f` to each entry in the set `s`. Each element
`x` in the old set is transformed into a new entry `x2`, where
the new value `x2` is created by applying `f` to `x`.
The result set may be smaller than the original set due to duplicate elements.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 1, 2, 3]));

func f(x : Nat) : Nat = if (x < 2) { x } else { 0 };

let resSet = natSet.map(set, f);

Debug.print(debug_show(Iter.toArray(natSet.vals(resSet))));
// [0, 1]
```

Cost of mapping all the elements:
Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory
where `n` denotes the number of elements stored in the set.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.


### Function `mapFilter`
``` motoko no-repl
func mapFilter<T1>(s : Set<T1>, f : T1 -> ?T) : Set<T>
```

Creates a new set by applying `f` to each element in the set `s`. For each element
`x` in the old set, if `f` evaluates to `null`, the element is discarded.
Otherwise, the entry is transformed into a new entry `x2`, where
the new value `x2` is the result of applying `f` to `x`.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 1, 2, 3]));

func f(x : Nat) : ?Nat {
  if(x == 0) {null}
  else { ?( x * 2 )}
};

let newRbSet = natSet.mapFilter(set, f);

Debug.print(debug_show(Iter.toArray(natSet.vals(newRbSet))));
// [2, 4, 6]
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.


### Function `isSubset`
``` motoko no-repl
func isSubset(s1 : Set<T>, s2 : Set<T>) : Bool
```

Test if `set1` is subset of `set2`.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set1 = natSet.fromIter(Iter.fromArray([1, 2]));
let set2 = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show natSet.isSubset(set1, set2)); // => true
```

Runtime: `O(m * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements stored in the sets set1 and set2, respectively,
and assuming that the `compare` function implements an `O(1)` comparison.


### Function `equals`
``` motoko no-repl
func equals(s1 : Set<T>, s2 : Set<T>) : Bool
```

Test if two sets are equal.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set1 = natSet.fromIter(Iter.fromArray([0, 2, 1]));
let set2 = natSet.fromIter(Iter.fromArray([1, 2]));

Debug.print(debug_show natSet.equals(set1, set1)); // => true
Debug.print(debug_show natSet.equals(set1, set2)); // => false
```

Runtime: `O(m * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements stored in the sets set1 and set2, respectively,
and assuming that the `compare` function implements an `O(1)` comparison.


### Function `vals`
``` motoko no-repl
func vals(s : Set<T>) : I.Iter<T>
```

Returns an Iterator (`Iter`) over the elements of the set.
Iterator provides a single method `next()`, which returns
elements in ascending order, or `null` when out of elements to iterate over.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show(Iter.toArray(natSet.vals(set))));
// [0, 1, 2]
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the set.

Note: Full set iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `valsRev`
``` motoko no-repl
func valsRev(s : Set<T>) : I.Iter<T>
```

Same as `vals()` but iterates over elements of the set `s` in the descending order.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show(Iter.toArray(natSet.valsRev(set))));
// [2, 1, 0]
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the set.

Note: Full set iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `empty`
``` motoko no-repl
func empty() : Set<T>
```

Create a new empty Set.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.empty();

Debug.print(debug_show(natSet.size(set))); // => 0
```

Cost of empty set creation
Runtime: `O(1)`.
Space: `O(1)`


### Function `size`
``` motoko no-repl
func size(s : Set<T>) : Nat
```

Returns the number of elements in the set.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show(natSet.size(set))); // => 3
```

Runtime: `O(1)`.
Space: `O(1)`.


### Function `foldLeft`
``` motoko no-repl
func foldLeft<Accum>(set : Set<T>, base : Accum, combine : (Accum, T) -> Accum) : Accum
```

Collapses the elements in `set` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
left to right.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

func folder(accum : Nat, val : Nat) : Nat = val + accum;

Debug.print(debug_show(natSet.foldLeft(set, 0, folder)));
// 3
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Space: depends on `combine` function plus garbage, see the note below.
where `n` denotes the number of elements stored in the set.

Note: Full set iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `foldRight`
``` motoko no-repl
func foldRight<Accum>(set : Set<T>, base : Accum, combine : (T, Accum) -> Accum) : Accum
```

Collapses the elements in `set` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
right to left.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

func folder(val : Nat, accum : Nat) : Nat = val + accum;

Debug.print(debug_show(natSet.foldRight(set, 0, folder)));
// 3
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Space: depends on `combine` function plus garbage, see the note below.
where `n` denotes the number of elements stored in the set.

Note: Full set iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `isEmpty`
``` motoko no-repl
func isEmpty(s : Set<T>) : Bool
```

Test if the given set `s` is empty.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.empty();

Debug.print(debug_show(natSet.isEmpty(set))); // => true
```

Runtime: `O(1)`.
Space: `O(1)`.


### Function `all`
``` motoko no-repl
func all(s : Set<T>, pred : T -> Bool) : Bool
```

Test whether all values in the set `s` satisfy a given predicate `pred`.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show(natSet.all(set, func (v) = (v < 10))));
// true
Debug.print(debug_show(natSet.all(set, func (v) = (v < 2))));
// false
```

Runtime: `O(n)`.
Space: `O(1)`.
where `n` denotes the number of elements stored in the set.


### Function `some`
``` motoko no-repl
func some(s : Set<T>, pred : (T) -> Bool) : Bool
```

Test if there exists an element in the set `s` satisfying the given predicate `pred`.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natSet = Set.Make<Nat>(Nat.compare);
let set = natSet.fromIter(Iter.fromArray([0, 2, 1]));

Debug.print(debug_show(natSet.some(set, func (v) = (v >= 3))));
// false
Debug.print(debug_show(natSet.some(set, func (v) = (v >= 0))));
// true
```

Runtime: `O(n)`.
Space: `O(1)`.
where `n` denotes the number of elements stored in the set.


### Function `validate`
``` motoko no-repl
func validate(s : Set<T>) : ()
```

Test helper that check internal invariant for the given set `s`. 
Raise an error (for a stack trace) if invariants are violated.

## Value `Make`
``` motoko no-repl
let Make : <T>(compare : (T, T) -> O.Order) -> Operations<T>
```

Create `OrderedSet.Operations` object capturing element type `T` and `compare` function. 
It is an alias for the `Operations` constructor.

Example:
```motoko
import Set "mo:base/OrderedSet";
import Nat "mo:base/Nat";

actor {
  let natSet = Set.Make<Nat>(Nat.compare);
  stable var set : Set.Set<Nat> = natSet.empty();
};
```
