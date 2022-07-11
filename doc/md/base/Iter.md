# Iter
Iterators

## Type `Iter`
`type Iter<T> = { next : () -> ?T }`

An iterator that produces values of type `T`. Calling `next` returns
`null` when iteration is finished.

Iterators are inherently stateful. Calling `next` "consumes" a value from
the Iterator that cannot be put back, so keep that in mind when sharing
iterators between consumers.

An iterater `i` can be iterated over using
```
for (x in i) {
  …do something with x…
}
```

## `class range`


### Function `next`
`func next() : ?Nat`

Creates an iterator that produces all `Nat`s from `x` to `y` including
both of the bounds.
```motoko
import Iter "mo:base/Iter";
let iter = Iter.range(1, 3);
assert(?1 == iter.next());
assert(?2 == iter.next());
assert(?3 == iter.next());
assert(null == iter.next());
```

## `class revRange`


### Function `next`
`func next() : ?Int`

Like `range` but produces the values in the opposite
order.

## Function `iterate`
`func iterate<A>(xs : Iter<A>, f : (A, Nat) -> ())`

Calls a function `f` on every value produced by an iterator and discards
the results. If you're looking to keep these results use `map` instead.

```motoko
import Iter "mo:base/Iter";
var sum = 0;
Iter.iterate<Nat>(Iter.range(1, 3), func(x, _index) {
  sum += x;
});
assert(6 == sum)
```

## Function `size`
`func size<A>(xs : Iter<A>) : Nat`

Consumes an iterator and counts how many elements were produced
(discarding them in the process).

## Function `map`
`func map<A, B>(xs : Iter<A>, f : A -> B) : Iter<B>`

Takes a function and an iterator and returns a new iterator that lazily applies
the function to every element produced by the argument iterator.
```motoko
import Iter "mo:base/Iter";
let iter = Iter.range(1, 3);
let mappedIter = Iter.map(iter, func (x : Nat) : Nat { x * 2 });
assert(?2 == mappedIter.next());
assert(?4 == mappedIter.next());
assert(?6 == mappedIter.next());
assert(null == mappedIter.next());
```

## Function `filter`
`func filter<A>(xs : Iter<A>, f : A -> Bool) : Iter<A>`

Takes a function and an iterator and returns a new iterator that produces
elements from the original iterator if and only if the predicate is true.
```motoko
import Iter "o:base/Iter";
let iter = Iter.range(1, 3);
let mappedIter = Iter.filter(iter, func (x : Nat) : Bool { x % 2 == 1 });
assert(?1 == mappedIter.next());
assert(?3 == mappedIter.next());
assert(null == mappedIter.next());
```

## Function `make`
`func make<A>(x : A) : Iter<A>`

Creates an iterator that produces an infinite sequence of `x`.
```motoko
import Iter "mo:base/Iter";
let iter = Iter.make(10);
assert(?10 == iter.next());
assert(?10 == iter.next());
assert(?10 == iter.next());
// ...
```

## Function `fromArray`
`func fromArray<A>(xs : [A]) : Iter<A>`

Creates an iterator that produces the elements of an Array in ascending index order.
```motoko
import Iter "mo:base/Iter";
let iter = Iter.fromArray([1, 2, 3]);
assert(?1 == iter.next());
assert(?2 == iter.next());
assert(?3 == iter.next());
assert(null == iter.next());
```

## Function `fromArrayMut`
`func fromArrayMut<A>(xs : [var A]) : Iter<A>`

Like `fromArray` but for Arrays with mutable elements. Captures
the elements of the Array at the time the iterator is created, so
further modifications won't be reflected in the iterator.

## Value `fromList`
`let fromList`

Like `fromArray` but for Lists.

## Function `toArray`
`func toArray<A>(xs : Iter<A>) : [A]`

Consumes an iterator and collects its produced elements in an Array.
```motoko
import Iter "mo:base/Iter";
let iter = Iter.range(1, 3);
assert([1, 2, 3] == Iter.toArray(iter));
```

## Function `toArrayMut`
`func toArrayMut<A>(xs : Iter<A>) : [var A]`

Like `toArray` but for Arrays with mutable elements.

## Function `toList`
`func toList<A>(xs : Iter<A>) : List.List<A>`

Like `toArray` but for Lists.
