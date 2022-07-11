# Deque
Functions for persistent, double-ended queues.

## Type `Deque`
`type Deque<T> = (List<T>, List<T>)`

Double-ended queue

## Function `empty`
`func empty<T>() : Deque<T>`

Empty queue

## Function `isEmpty`
`func isEmpty<T>(q : Deque<T>) : Bool`

True when the queue is empty

## Function `pushFront`
`func pushFront<T>(q : Deque<T>, x : T) : Deque<T>`

Insert a new element on the front end of the queue

## Function `peekFront`
`func peekFront<T>(q : Deque<T>) : ?T`

Inspect the (optional) first element on the front end of the queue

## Function `popFront`
`func popFront<T>(q : Deque<T>) : ?(T, Deque<T>)`

Remove the first element on the front end of the queue; Returns null when empty.

## Function `pushBack`
`func pushBack<T>(q : Deque<T>, x : T) : Deque<T>`

Insert a new element on the back end of the queue

## Function `peekBack`
`func peekBack<T>(q : Deque<T>) : ?T`

Inspect the (optional) first element on the back end of the queue

## Function `popBack`
`func popBack<T>(q : Deque<T>) : ?(Deque<T>, T)`

Remove the first element on the back end of the queue; Returns null when empty.
