# Stack
Class `Stack<X>` provides a Minimal LIFO stack of elements of type `X`.

See library `Deque` for mixed LIFO/FIFO behavior.

Example:
```motoko name=initialize
import Stack "mo:base/Stack";

let stack = Stack.Stack<Nat>(); // create a stack
```
Runtime: O(1)

Space: O(1)

## Class `Stack<T>`

``` motoko no-repl
class Stack<T>()
```


### Function `push`
``` motoko no-repl
func push(x : T)
```

Push an element on the top of the stack.

Example:
```motoko include=initialize
stack.push(1);
stack.push(2);
stack.push(3);
stack.peek(); // examine the top most element
```

Runtime: O(1)

Space: O(1)


### Function `isEmpty`
``` motoko no-repl
func isEmpty() : Bool
```

True when the stack is empty and false otherwise.

Example:
```motoko include=initialize
stack.isEmpty();
```

Runtime: O(1)

Space: O(1)


### Function `peek`
``` motoko no-repl
func peek() : ?T
```

Return (without removing) the top element, or return null if the stack is empty.

Example:
```motoko include=initialize
stack.push(1);
stack.push(2);
stack.push(3);
stack.peek();
```

Runtime: O(1)

Space: O(1)


### Function `pop`
``` motoko no-repl
func pop() : ?T
```

Remove and return the top element, or return null if the stack is empty.

Example:
```motoko include=initialize
stack.push(1);
ignore stack.pop();
stack.isEmpty();
```

Runtime: O(1)

Space: O(1)
