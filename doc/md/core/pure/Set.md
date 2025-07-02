# core/pure/Set
Pure (immutable) sets based on order/comparison of elements.
A set is a collection of elements without duplicates.
The set data structure type is stable and can be used for orthogonal persistence.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter([3, 1, 2, 3].values(), Nat.compare);
  assert Set.size(set) == 3;
  assert not Set.contains(set, Nat.compare, 4);
  let diff = Set.difference(set, set, Nat.compare);
  assert Set.isEmpty(diff);
}
```

These sets are implemented as red-black trees, a balanced binary search tree of ordered elements.

The tree data structure internally colors each of its nodes either red or black,
and uses this information to balance the tree during modifying operations.

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
type Set<T> = Types.Pure.Set<T>
```

Ordered collection of unique elements of the generic type `T`.
If type `T` is stable then `Set<T>` is also stable.
To ensure that property the `Set<T>` does not have any methods,
instead they are gathered in the functor-like class `Operations` (see example there).

## Function `fromIter`
``` motoko no-repl
func fromIter<T>(iter : Iter.Iter<T>, compare : (T, T) -> Order.Order) : Set<T>
```

Create a set with the elements obtained from an iterator.
Potential duplicate elements in the iterator are ignored, i.e.
multiple occurrences of an equal element only occur once in the set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set = Set.fromIter([3, 1, 2, 1].values(), Nat.compare);
  assert Iter.toArray(Set.values(set)) == [1, 2, 3];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)`.
where `n` denotes the number of elements returned by the iterator and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.

## Function `add`
``` motoko no-repl
func add<T>(set : Set<T>, compare : (T, T) -> Order.Order, elem : T) : Set<T>
```

Given a `set` ordered by `compare`, insert the new `element`,
returning the new set.

Return the set unchanged if the element already exists in the set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set0 = Set.empty<Nat>();
  let set1 = Set.add(set0, Nat.compare, 2);
  let set2 = Set.add(set1, Nat.compare, 1);
  let set3 = Set.add(set2, Nat.compare, 2);
  assert Iter.toArray(Set.values(set0)) == [];
  assert Iter.toArray(Set.values(set1)) == [2];
  assert Iter.toArray(Set.values(set2)) == [1, 2];
  assert Iter.toArray(Set.values(set3)) == [1, 2];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned set shares with the `set` most of the tree nodes.
Garbage collecting one of the sets (e.g. after an assignment `m := Set.add(m, c, e)`)
causes collecting `O(log(n))` nodes.

## Function `insert`
``` motoko no-repl
func insert<T>(set : Set<T>, compare : (T, T) -> Order.Order, elem : T) : (Set<T>, Bool)
```

Given `set` ordered by `compare`, insert the new `element`,
returning the set extended with `element` and a Boolean indicating
if the element was already present in `set`.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set0 = Set.empty();
  do {
    let (set1, new1) = Set.insert(set0, Nat.compare, 2);
    assert new1;
    let (set2, new2) = Set.insert(set1, Nat.compare, 1);
    assert new2;
    let (set3, new3) = Set.insert(set2, Nat.compare, 2);
    assert not new3;
    assert Iter.toArray(Set.values(set3)) == [1, 2]
  }
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned set shares with the `set` most of the tree nodes.
Garbage collecting one of the sets (e.g. after an assignment `m := Set.add(m, c, e)`)
causes collecting `O(log(n))` nodes.

## Function `remove`
``` motoko no-repl
func remove<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : Set<T>
```

Given `set` ordered by `compare` return the set with `element` removed.
Return the set unchanged if the element was absent.

```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set = Set.fromIter([1, 2, 3].values(), Nat.compare);

  let set1 = Set.remove(set, Nat.compare, 2);
  let set2 = Set.remove(set1, Nat.compare, 4);
  assert Iter.toArray(Set.values(set2)) == [1, 3];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))` including garbage, see below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` objects that will be collected as garbage.
Note: The returned set shares with `set` most of the tree nodes.
Garbage collecting one of the sets (e.g. after an assignment `m := Set.delete(m, c, e)`)
causes collecting `O(log(n))` nodes.

## Function `delete`
``` motoko no-repl
func delete<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : (Set<T>, Bool)
```

Given `set` ordered by `compare`, delete `element` from the set, returning
either the set without the element and a Boolean indicating whether
whether `element` was contained in `set`.

```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set = Set.fromIter([1, 2, 3].values(), Nat.compare);
  do {
    let (set1, contained1) = Set.delete(set, Nat.compare, 2);
    assert contained1;
    assert Iter.toArray(Set.values(set1)) == [1, 3];
    let (set2, contained2) = Set.delete(set1, Nat.compare, 4);
    assert not contained2;
    assert Iter.toArray(Set.values(set2)) == [1, 3];
  }
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))` including garbage, see below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` objects that will be collected as garbage.
Note: The returned set shares with `set` most of the tree nodes.
Garbage collecting one of the sets (e.g. after an assignment `m := Set.delete(m, c, e)`)
causes collecting `O(log(n))` nodes.

## Function `contains`
``` motoko no-repl
func contains<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : Bool
```

Tests whether the set contains the provided element.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Bool "mo:core/Bool";

persistent actor {
  let set = Set.fromIter([3, 1, 2].values(), Nat.compare);

  assert Set.contains(set, Nat.compare, 1);
  assert not Set.contains(set, Nat.compare, 4);
}
```

Runtime: `O(log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `max`
``` motoko no-repl
func max<T>(s : Set<T>) : ?T
```

Get the maximal element of the set `set` if it is not empty, otherwise returns `null`

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set1 = Set.fromIter<Nat>([0, 2, 1].values(), Nat.compare);
  let set2 = Set.empty<Nat>();
  assert Set.max(set1) == ?2;
  assert Set.max(set2) == null;
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of elements in the set

## Function `min`
``` motoko no-repl
func min<T>(s : Set<T>) : ?T
```

Retrieves the minimum element from the set.
If the set is empty, returns `null`.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set1 = Set.fromIter<Nat>([2, 0, 1].values(), Nat.compare);
  let set2 = Set.empty<Nat>();
  assert Set.min(set1) == ?0;
  assert Set.min(set2) == null;
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of elements stored in the set.

## Function `union`
``` motoko no-repl
func union<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T>
```

Returns a new set that is the union of `set1` and `set2`,
i.e. a new set that all the elements that exist in at least on of the two sets.
Potential duplicates are ignored, i.e. if the same element occurs in both `set1`
and `set2`, it only occurs once in the returned set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  let union = Set.union(set1, set2, Nat.compare);
  assert Iter.toArray(Set.values(union)) == [1, 2, 3, 4, 5];
}
```

Runtime: `O(m * log(n))`.
Space: `O(m)`, retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements in the sets, and `m <= n`.
and assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(m * log(n))` temporary objects that will be collected as garbage.

## Function `intersection`
``` motoko no-repl
func intersection<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T>
```

Returns a new set that is the intersection of `set1` and `set2`,
i.e. a new set that contains all the elements that exist in both sets.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set1 = Set.fromIter([0, 1, 2].values(), Nat.compare);
  let set2 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  let intersection = Set.intersection(set1, set2, Nat.compare);
  assert Iter.toArray(Set.values(intersection)) == [1, 2];
}
```

Runtime: `O(m * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements stored in the sets `set1` and `set2`, respectively,
and assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(m)` temporary objects that will be collected as garbage.

## Function `difference`
``` motoko no-repl
func difference<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T>
```

Returns a new set that is the difference between `set1` and `set2` (`set1` minus `set2`),
i.e. a new set that contains all the elements of `set1` that do not exist in `set2`.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  let difference = Set.difference(set1, set2, Nat.compare);
  assert Iter.toArray(Set.values(difference)) == [1, 2];
}
```

Runtime: `O(m * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements stored in the sets `set1` and `set2`, respectively,
and assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(m * log(n))` temporary objects that will be collected as garbage.

## Function `map`
``` motoko no-repl
func map<T1, T2>(s : Set<T1>, compare : (T2, T2) -> Order.Order, project : T1 -> T2) : Set<T2>
```

Project all elements of the set in a new set.
Apply a mapping function to each element in the set and
collect the mapped elements in a new mutable set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Text "mo:core/Text";
import Iter "mo:core/Iter";

persistent actor {
  let numbers = Set.fromIter([3, 1, 2].values(), Nat.compare);

  let textNumbers =
    Set.map<Nat, Text>(numbers, Text.compare, Nat.toText);
  assert Iter.toArray(Set.values(textNumbers)) == ["1", "2", "3"];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.

## Function `forEach`
``` motoko no-repl
func forEach<T>(set : Set<T>, operation : T -> ())
```

Apply an operation on each element contained in the set.
The operation is applied in ascending order of the elements.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let numbers = Set.fromIter([0, 3, 1, 2].values(), Nat.compare);

  var text = "";
  Set.forEach<Nat>(numbers, func (element) {
    text #= " " # Nat.toText(element)
  });
  assert text == " 0 1 2 3";
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory.
where `n` denotes the number of elements stored in the set.


## Function `filter`
``` motoko no-repl
func filter<T>(set : Set<T>, compare : (T, T) -> Order.Order, criterion : T -> Bool) : Set<T>
```

Filter elements in a new set.
Create a copy of the mutable set that only contains the elements
that fulfil the criterion function.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let numbers = Set.fromIter([0, 3, 1, 2].values(), Nat.compare);

  let evenNumbers = Set.filter<Nat>(numbers, Nat.compare, func (number) {
    number % 2 == 0
  });
  assert Iter.toArray(Set.values(evenNumbers)) == [0, 2];
}
```

Runtime: `O(n)`.
Space: `O(n)`.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `filterMap`
``` motoko no-repl
func filterMap<T1, T2>(set : Set<T1>, compare : (T2, T2) -> Order.Order, project : T1 -> ?T2) : Set<T2>
```

Filter all elements in the set by also applying a projection to the elements.
Apply a mapping function `project` to all elements in the set and collect all
elements, for which the function returns a non-null new element. Collect all
non-discarded new elements in a new mutable set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Text "mo:core/Text";
import Iter "mo:core/Iter";

persistent actor {
  let numbers = Set.fromIter([3, 0, 2, 1].values(), Nat.compare);

  let evenTextNumbers = Set.filterMap<Nat, Text>(numbers, Text.compare, func (number) {
    if (number % 2 == 0) {
       ?Nat.toText(number)
    } else {
       null // discard odd numbers
    }
  });
  assert Iter.toArray(Set.values(evenTextNumbers)) == ["0", "2"];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the set and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.

## Function `isSubset`
``` motoko no-repl
func isSubset<T>(s1 : Set<T>, s2 : Set<T>, compare : (T, T) -> Order.Order) : Bool
```

Test whether `set1` is a sub-set of `set2`, i.e. each element in `set1` is
also contained in `set2`. Returns `true` if both sets are equal.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set1 = Set.fromIter([1, 2].values(), Nat.compare);
  let set2 = Set.fromIter([2, 1, 0].values(), Nat.compare);
  let set3 = Set.fromIter([3, 4].values(), Nat.compare);
  assert Set.isSubset(set1, set2, Nat.compare);
  assert not Set.isSubset(set1, set3, Nat.compare);
}
```

Runtime: `O(m * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements stored in the sets set1 and set2, respectively,
and assuming that the `compare` function implements an `O(1)` comparison.

## Function `equal`
``` motoko no-repl
func equal<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Bool
```

Test whether two sets are equal.
Both sets have to be constructed by the same comparison function.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set1 = Set.fromIter([1, 2].values(), Nat.compare);
  let set2 = Set.fromIter([2, 1].values(), Nat.compare);
  let set3 = Set.fromIter([2, 1, 0].values(), Nat.compare);
  assert Set.equal(set1, set2, Nat.compare);
  assert not Set.equal(set1, set3, Nat.compare);
}
```

Runtime: `O(m * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `m` and `n` denote the number of elements stored in the sets set1 and set2, respectively,
and assuming that the `compare` function implements an `O(1)` comparison.

## Function `compare`
``` motoko no-repl
func compare<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Order.Order
```

Compare two sets by comparing the elements.
Both sets must have been created by the same comparison function.
The two sets are iterated by the ascending order of their creation and
order is determined by the following rules:
Less:
`set1` is less than `set2` if:
 * the pairwise iteration hits an element pair `element1` and `element2` where
   `element1` is less than `element2` and all preceding elements are equal, or,
 * `set1` is  a strict prefix of `set2`, i.e. `set2` has more elements than `set1`
    and all elements of `set1` occur at the beginning of iteration `set2`.
Equal:
`set1` and `set2` have same series of equal elements by pairwise iteration.
Greater:
`set1` is neither less nor equal `set2`.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set1 = Set.fromIter([0, 1].values(), Nat.compare);
  let set2 = Set.fromIter([0, 2].values(), Nat.compare);

  assert Set.compare(set1, set2, Nat.compare) == #less;
  assert Set.compare(set1, set1, Nat.compare) == #equal;
  assert Set.compare(set2, set1, Nat.compare) == #greater;
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set and
assuming that `compare` has runtime and space costs of `O(1)`.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `values`
``` motoko no-repl
func values<T>(set : Set<T>) : Iter.Iter<T>
```

Returns an iterator over the elements in the set,
traversing the elements in the ascending order.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter([0, 2, 3, 1].values(), Nat.compare);

  var text = "";
  for (number in Set.values(set)) {
     text #= " " # Nat.toText(number);
  };
  assert text == " 0 1 2 3";
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `reverseValues`
``` motoko no-repl
func reverseValues<T>(set : Set<T>) : Iter.Iter<T>
```

Returns an iterator over the elements in the set,
traversing the elements in the descending order.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter([0, 2, 3, 1].values(), Nat.compare);

  var tmp = "";
  for (number in Set.reverseValues(set)) {
     tmp #= " " # Nat.toText(number);
  };
  assert tmp == " 3 2 1 0";
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `empty`
``` motoko no-repl
func empty<T>() : Set<T>
```

Create a new empty set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set = Set.empty<Nat>();
  assert Iter.toArray(Set.values(set)) == [];
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `singleton`
``` motoko no-repl
func singleton<T>(element : T) : Set<T>
```

Create a new set with a single element.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set = Set.singleton(0);
  assert Iter.toArray(Set.values(set)) == [0];
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `size`
``` motoko no-repl
func size<T>(set : Set<T>) : Nat
```

Return the number of elements in a set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter([0, 3, 2, 1, 3].values(), Nat.compare);

  assert Set.size(set) == 4;
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `foldLeft`
``` motoko no-repl
func foldLeft<T, A>(set : Set<T>, base : A, combine : (A, T) -> A) : A
```

Iterate all elements in ascending order,
and accumulate the elements by applying the combine function, starting from a base value.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter([0, 3, 2, 1].values(), Nat.compare);

  let text = Set.foldLeft<Nat, Text>(
     set,
     "",
     func (accumulator, element) {
       accumulator # " " # Nat.toText(element)
     }
  );
  assert text == " 0 1 2 3";
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set.

## Function `foldRight`
``` motoko no-repl
func foldRight<T, A>(set : Set<T>, base : A, combine : (T, A) -> A) : A
```

Iterate all elements in descending order,
and accumulate the elements by applying the combine function, starting from a base value.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter([0, 3, 2, 1].values(), Nat.compare);

  let text = Set.foldRight<Nat, Text>(
     set,
     "",
     func (element, accumulator) {
        accumulator # " " # Nat.toText(element)
     }
  );
  assert text == " 3 2 1 0";
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set.

## Function `isEmpty`
``` motoko no-repl
func isEmpty<T>(set : Set<T>) : Bool
```

Determines whether a set is empty.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set1 = Set.empty<Nat>();
  let set2 = Set.singleton<Nat>(1);

  assert Set.isEmpty(set1);
  assert not Set.isEmpty(set2);
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `all`
``` motoko no-repl
func all<T>(set : Set<T>, predicate : T -> Bool) : Bool
```

Check whether all element in the set satisfy a predicate, i.e.
the `predicate` function returns `true` for all elements in the set.
Returns `true` for an empty set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter<Nat>([0, 3, 1, 2].values(), Nat.compare);

  let belowTen = Set.all<Nat>(set, func (number) {
    number < 10
  });
  assert belowTen;
}
```

Runtime: `O(n)`.
Space: `O(1)`.
where `n` denotes the number of elements stored in the set.

## Function `any`
``` motoko no-repl
func any<T>(s : Set<T>, pred : T -> Bool) : Bool
```

Check whether at least one element in the set satisfies a predicate, i.e.
the `predicate` function returns `true` for at least one element in the set.
Returns `false` for an empty set.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";

persistent actor {
  let set = Set.fromIter<Nat>([0, 3, 1, 2].values(), Nat.compare);

  let aboveTen = Set.any<Nat>(set, func (number) {
    number > 10
  });
  assert not aboveTen;
}
```

Runtime: `O(n)`.
Space: `O(1)`.

## Function `assertValid`
``` motoko no-repl
func assertValid<T>(set : Set<T>, compare : (T, T) -> Order.Order) : ()
```

Test helper that check internal invariant for the given set `s`.
Raise an error (for a stack trace) if invariants are violated.

## Function `toText`
``` motoko no-repl
func toText<T>(set : Set<T>, elementFormat : T -> Text) : Text
```

Generate a textual representation of all the elements in the set.
Primarily to be used for testing and debugging.
The elements are formatted according to `elementFormat`.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set = Set.fromIter<Nat>([0, 3, 1, 2].values(), Nat.compare);

  assert Set.toText(set, Nat.toText) == "PureSet{0, 1, 2, 3}";
}
```

Runtime: `O(n)`.
Space: `O(n)` retained memory plus garbage, see below.
where `n` denotes the number of elements stored in the set and
assuming that `elementFormat` has runtime and space costs of `O(1)`.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `flatten`
``` motoko no-repl
func flatten<T>(setOfSets : Set<Set<T>>, compare : (T, T) -> Order.Order) : Set<T>
```

Construct the union of a set of element sets, i.e. all elements of
each element set are included in the result set.
Any duplicates are ignored, i.e. if the same element occurs in multiple element sets,
it only occurs once in the result set.

Assumes all sets are ordered by `compare`.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Order "mo:core/Order";
import Iter "mo:core/Iter";

persistent actor {
  func setCompare(first: Set.Set<Nat>, second: Set.Set<Nat>) : Order.Order {
     Set.compare(first, second, Nat.compare)
  };

  let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  let set3 = Set.fromIter([5, 6, 7].values(), Nat.compare);
  let setOfSets = Set.fromIter([set1, set2, set3].values(), setCompare);
  let flatSet = Set.flatten(setOfSets, Nat.compare);
  assert Iter.toArray(Set.values(flatSet)) == [1, 2, 3, 4, 5, 6, 7];
}
```

Runtime: `O(n * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in all the sub-sets,
and assuming that the `compare` function implements an `O(1)` comparison.

## Function `join`
``` motoko no-repl
func join<T>(setIterator : Iter.Iter<Set<T>>, compare : (T, T) -> Order.Order) : Set<T>
```

Construct the union of a series of sets, i.e. all elements of
each set are included in the result set.
Any duplicates are ignored, i.e. if an element occurs
in several of the iterated sets, it only occurs once in the result set.

Assumes all sets are ordered by `compare`.

Example:
```motoko
import Set "mo:core/pure/Set";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  let set3 = Set.fromIter([5, 6, 7].values(), Nat.compare);
  let combined = Set.join([set1, set2, set3].values(), Nat.compare);
  assert Iter.toArray(Set.values(combined)) == [1, 2, 3, 4, 5, 6, 7];
}
```

Runtime: `O(n * log(n))`.
Space: `O(1)` retained memory plus garbage, see the note below.
where `n` denotes the number of elements stored in the iterated sets,
and assuming that the `compare` function implements an `O(1)` comparison.
