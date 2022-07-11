# Stack
Stack collection (LIFO discipline).

Minimal LIFO (last in first out) implementation, as a class.
See library `Deque` for mixed LIFO/FIFO behavior.


## `class Stack<T>`


### Function `push`
``` motoko norepl
func push(x : T)
```

Push an element on the top of the stack.


### Function `isEmpty`
``` motoko norepl
func isEmpty() : Bool
```

True when the stack is empty.


### Function `peek`
``` motoko norepl
func peek() : ?T
```

Return and retain the top element, or return null.


### Function `pop`
``` motoko norepl
func pop() : ?T
```

Remove and return the top element, or return null.
