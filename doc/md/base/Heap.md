# Heap
Priority Queue

This module provides purely-functional priority queue based on leftist heap

## Type `Tree`
``` motoko
type Tree<T> = ?(Int, T, Tree<T>, Tree<T>)
```


## `class Heap<T>`


### Function `share`
``` motoko
func share() : Tree<T>
```

Get purely-functional representation


### Function `unsafeUnshare`
``` motoko
func unsafeUnshare(t : Tree<T>)
```

Put purely-functional representation into class. Need to make sure the tree is constructed with the same compare function


### Function `put`
``` motoko
func put(x : T)
```

Insert an element to the heap


### Function `peekMin`
``` motoko
func peekMin() : ?T
```

Return the minimal element


### Function `deleteMin`
``` motoko
func deleteMin()
```

Delete the minimal element


### Function `removeMin`
``` motoko
func removeMin() : ?T
```

Remove the minimal element and return its value

## Function `fromIter`
``` motoko
func fromIter<T>(iter : I.Iter<T>, ord : (T, T) -> O.Order) : Heap<T>
```

Convert iterator into a heap in O(N) time.
