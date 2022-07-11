# Deque
Functions for persistent, double-ended queues.

## Type `Deque`

``` motoko
type Deque<T> = (List<T>, List<T>)
```

Double-ended queue

## Function `empty`

``` motoko
func empty<T>() : Deque<T>
```

Empty queue

## Function `isEmpty`

``` motoko
func isEmpty<T>(q : Deque<T>) : Bool
```

True when the queue is empty

## Function `pushFront`

``` motoko
func pushFront<T>(q : Deque<T>, x : T) : Deque<T>
```

Insert a new element on the front end of the queue

## Function `peekFront`

``` motoko
func peekFront<T>(q : Deque<T>) : ?T
```

Inspect the (optional) first element on the front end of the queue

## Function `popFront`

``` motoko
func popFront<T>(q : Deque<T>) : ?(T, Deque<T>)
```

Remove the first element on the front end of the queue; Returns null when empty.

## Function `pushBack`

``` motoko
func pushBack<T>(q : Deque<T>, x : T) : Deque<T>
```

Insert a new element on the back end of the queue

## Function `peekBack`

``` motoko
func peekBack<T>(q : Deque<T>) : ?T
```

Inspect the (optional) first element on the back end of the queue

## Function `popBack`

``` motoko
func popBack<T>(q : Deque<T>) : ?(Deque<T>, T)
```

Remove the first element on the back end of the queue; Returns null when empty.
