# Deque
Functions for persistent, double-ended queues.

## Type `Deque`
``` motoko norepl
type Deque<T> = (List<T>, List<T>)
```

Double-ended queue

## Function `empty`
``` motoko norepl
func empty<T>() : Deque<T>
```

Empty queue

## Function `isEmpty`
``` motoko norepl
func isEmpty<T>(q : Deque<T>) : Bool
```

True when the queue is empty

## Function `pushFront`
``` motoko norepl
func pushFront<T>(q : Deque<T>, x : T) : Deque<T>
```

Insert a new element on the front end of the queue

## Function `peekFront`
``` motoko norepl
func peekFront<T>(q : Deque<T>) : ?T
```

Inspect the (optional) first element on the front end of the queue

## Function `popFront`
``` motoko norepl
func popFront<T>(q : Deque<T>) : ?(T, Deque<T>)
```

Remove the first element on the front end of the queue; Returns null when empty.

## Function `pushBack`
``` motoko norepl
func pushBack<T>(q : Deque<T>, x : T) : Deque<T>
```

Insert a new element on the back end of the queue

## Function `peekBack`
``` motoko norepl
func peekBack<T>(q : Deque<T>) : ?T
```

Inspect the (optional) first element on the back end of the queue

## Function `popBack`
``` motoko norepl
func popBack<T>(q : Deque<T>) : ?(Deque<T>, T)
```

Remove the first element on the back end of the queue; Returns null when empty.
