# Deque
Functions for persistent, double-ended queues.

## Type `Deque`
``` motoko no-repl
type Deque<T> = (List<T>, List<T>)
```

Double-ended queue

## Function `empty`
``` motoko no-repl
func empty<T>() : Deque<T>
```

Empty queue

## Function `isEmpty`
``` motoko no-repl
func isEmpty<T>(q : Deque<T>) : Bool
```

True when the queue is empty

## Function `pushFront`
``` motoko no-repl
func pushFront<T>(q : Deque<T>, x : T) : Deque<T>
```

Insert a new element on the front end of the queue

## Function `peekFront`
``` motoko no-repl
func peekFront<T>(q : Deque<T>) : ?T
```

Inspect the (optional) first element on the front end of the queue

## Function `popFront`
``` motoko no-repl
func popFront<T>(q : Deque<T>) : ?(T, Deque<T>)
```

Remove the first element on the front end of the queue; Returns null when empty.

## Function `pushBack`
``` motoko no-repl
func pushBack<T>(q : Deque<T>, x : T) : Deque<T>
```

Insert a new element on the back end of the queue

## Function `peekBack`
``` motoko no-repl
func peekBack<T>(q : Deque<T>) : ?T
```

Inspect the (optional) first element on the back end of the queue

## Function `popBack`
``` motoko no-repl
func popBack<T>(q : Deque<T>) : ?(Deque<T>, T)
```

Remove the first element on the back end of the queue; Returns null when empty.
