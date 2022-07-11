# Stack
Stack collection (LIFO discipline).

Minimal LIFO (last in first out) implementation, as a class.
See library `Deque` for mixed LIFO/FIFO behavior.


## `class Stack<T>`


### Function `push`
``` motoko
func push(x : T)
```

Push an element on the top of the stack.


### Function `isEmpty`
``` motoko
func isEmpty() : Bool
```

True when the stack is empty.


### Function `peek`
``` motoko
func peek() : ?T
```

Return and retain the top element, or return null.


### Function `pop`
``` motoko
func pop() : ?T
```

Remove and return the top element, or return null.
