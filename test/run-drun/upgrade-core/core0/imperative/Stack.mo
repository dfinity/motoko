/// A mutable stack data structure.
/// Elements can be pushed on top of the stack
/// and removed from top of the stack (LIFO).
///
/// Example:
/// ```motoko
/// import Stack "mo:core/Stack";
/// import Debug "mo:core/Debug";
///
/// persistent actor {
///   let levels = Stack.empty<Text>();
///   Stack.push(levels, "Inner");
///   Stack.push(levels, "Middle");
///   Stack.push(levels, "Outer");
///   assert Stack.pop(levels) == ?"Outer";
///   assert Stack.pop(levels) == ?"Middle";
///   assert Stack.pop(levels) == ?"Inner";
///   assert Stack.pop(levels) == null;
/// }
/// ```
///
/// The internal implementation is a singly-linked list.
///
/// Performance:
/// * Runtime: `O(1)` for push, pop, and peek operation.
/// * Space: `O(n)`.
/// `n` denotes the number of elements stored on the stack.

// TODO: optimize or re-use pure/List operations (e.g. for `any` etc)

import Order "../Order";
import Types "../Types";
import PureList "../pure/List";

module {
  type List<T> = Types.Pure.List<T>;
  public type Stack<T> = Types.Stack<T>;

  /// Convert a mutable stack to an immutable, purely functional list.
  /// Please note that functional lists are ordered like stacks (FIFO).
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import PureList "mo:core/pure/List";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let mutableStack = Stack.empty<Nat>();
  ///   Stack.push(mutableStack, 3);
  ///   Stack.push(mutableStack, 2);
  ///   Stack.push(mutableStack, 1);
  ///   let immutableList = Stack.toPure(mutableStack);
  ///   assert Iter.toArray(PureList.values(immutableList)) == [1, 2, 3];
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of elements stored in the stack.
  public func toPure<T>(stack : Stack<T>) : PureList.List<T> {
    stack.top
  };

  /// Convert an immutable, purely functional list to a mutable stack.
  /// Please note that functional lists are ordered like stacks (FIFO).
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import PureList "mo:core/pure/List";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let immutableList = PureList.fromIter<Nat>([1, 2, 3].values());
  ///   let mutableStack = Stack.fromPure<Nat>(immutableList);
  ///   assert Iter.toArray(Stack.values(mutableStack)) == [1, 2, 3];
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of elements stored in the queue.
  public func fromPure<T>(list : PureList.List<T>) : Stack<T> {
    var size = 0;
    var cur = list;
    loop {
      switch cur {
        case (?(_, next)) {
          size += 1;
          cur := next
        };
        case null {
          return { var top = list; var size }
        }
      }
    }
  };

  /// Create a new empty mutable stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Text>();
  ///   assert Stack.size(stack) == 0;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func empty<T>() : Stack<T> {
    {
      var top = null;
      var size = 0
    }
  };

  /// Creates a new stack with `size` elements by applying the `generator` function to indices `[0..size-1]`.
  /// Elements are pushed in ascending index order.
  /// Which means that the generated element with the index `0` will be at the bottom of the stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let stack = Stack.tabulate<Nat>(3, func(i) { 2 * i });
  ///   assert Iter.toArray(Stack.values(stack)) == [4, 2, 0];
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `generator` has O(1) costs.
  public func tabulate<T>(size : Nat, generator : Nat -> T) : Stack<T> {
    let stack = empty<T>();
    var index = 0;
    while (index < size) {
      let element = generator(index);
      push(stack, element);
      index += 1
    };
    stack
  };

  /// Creates a new stack containing a single element.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.singleton<Text>("motoko");
  ///   assert Stack.peek(stack) == ?"motoko";
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func singleton<T>(element : T) : Stack<T> {
    let stack = empty<T>();
    push(stack, element);
    stack
  };

  /// Removes all elements from the stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   Stack.clear(stack);
  ///   assert Stack.isEmpty(stack);
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func clear<T>(stack : Stack<T>) {
    stack.top := null;
    stack.size := 0
  };

  /// Creates a deep copy of the stack with the same elements in the same order.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let original = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   let copy = Stack.clone(original);
  ///   assert Stack.equal(copy, original, Nat.equal);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of elements stored on the stack.
  public func clone<T>(stack : Stack<T>) : Stack<T> {
    let copy = empty<T>();
    for (element in values(stack)) {
      push(copy, element)
    };
    reverse(copy);
    copy
  };

  /// Returns true if the stack contains no elements.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   assert Stack.isEmpty(stack);
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func isEmpty<T>(stack : Stack<T>) : Bool {
    stack.size == 0
  };

  /// Returns the number of elements on the stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   assert Stack.size(stack) == 3;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func size<T>(stack : Stack<T>) : Nat {
    stack.size
  };

  /// Returns true if the stack contains the specified element.
  /// Uses the provided equality function to compare elements.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let stack = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   assert Stack.contains(stack, Nat.equal, 2);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack and assuming
  /// that `equal` has O(1) costs.
  public func contains<T>(stack : Stack<T>, equal : (T, T) -> Bool, element : T) : Bool {
    for (existing in values(stack)) {
      if (equal(existing, element)) {
        return true
      }
    };
    false
  };

  /// Pushes a new element onto the top of the stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 42);
  ///   assert Stack.peek(stack) == ?42;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func push<T>(stack : Stack<T>, value : T) {
    stack.top := ?(value, stack.top);
    stack.size += 1
  };

  /// Returns the top element of the stack without removing it.
  /// Returns null if the stack is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   assert Stack.peek(stack) == ?1;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func peek<T>(stack : Stack<T>) : ?T {
    switch (stack.top) {
      case null null;
      case (?(value, _)) ?value
    }
  };

  /// Removes and returns the top element of the stack.
  /// Returns null if the stack is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   assert Stack.pop(stack) == ?1;
  ///   assert Stack.pop(stack) == ?2;
  ///   assert Stack.pop(stack) == ?3;
  ///   assert Stack.pop(stack) == null;
  /// }
  /// ```
  ///
  /// Runtime: O(1)
  /// Space: O(1)
  public func pop<T>(stack : Stack<T>) : ?T {
    switch (stack.top) {
      case null null;
      case (?(value, next)) {
        stack.top := next;
        stack.size -= 1;
        ?value
      }
    }
  };

  /// Returns the element at the specified position from the top of the stack.
  /// Returns null if position is out of bounds.
  /// Position 0 is the top of the stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Char>();
  ///   Stack.push(stack, 'c');
  ///   Stack.push(stack, 'b');
  ///   Stack.push(stack, 'a');
  ///   assert Stack.get(stack, 0) == ?'a';
  ///   assert Stack.get(stack, 1) == ?'b';
  ///   assert Stack.get(stack, 2) == ?'c';
  ///   assert Stack.get(stack, 3) == null;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack.
  public func get<T>(stack : Stack<T>, position : Nat) : ?T {
    var index = 0;
    var current = stack.top;
    while (index < position) {
      switch (current) {
        case null return null;
        case (?(_, next)) {
          current := next
        }
      };
      index += 1
    };
    switch (current) {
      case null null;
      case (?(value, _)) ?value
    }
  };

  /// Reverses the order of elements in the stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   Stack.reverse(stack);
  ///   assert Stack.pop(stack) == ?3;
  ///   assert Stack.pop(stack) == ?2;
  ///   assert Stack.pop(stack) == ?1;
  ///   assert Stack.pop(stack) == null;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of elements stored on the stack.
  public func reverse<T>(stack : Stack<T>) {
    var last : List<T> = null;
    for (element in values(stack)) {
      last := ?(element, last)
    };
    stack.top := last
  };

  /// Returns an iterator over the elements in the stack, from top to bottom.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   assert Iter.toArray(Stack.values(stack)) == [1, 2, 3];
  /// }
  /// ```
  ///
  /// Runtime: O(1) for iterator creation, O(n) for full traversal
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack.
  public func values<T>(stack : Stack<T>) : Types.Iter<T> {
    object {
      var current = stack.top;

      public func next() : ?T {
        switch (current) {
          case null null;
          case (?(value, next)) {
            current := next;
            ?value
          }
        }
      }
    }
  };

  /// Returns true if all elements in the stack satisfy the predicate.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.fromIter<Nat>([2, 4, 6].values());
  ///   assert Stack.all<Nat>(stack, func(n) = n % 2 == 0);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `predicate` has O(1) costs.
  public func all<T>(stack : Stack<T>, predicate : T -> Bool) : Bool {
    for (element in values(stack)) {
      if (not predicate(element)) {
        return false
      }
    };
    true
  };

  /// Returns true if any element in the stack satisfies the predicate.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   assert Stack.any<Nat>(stack, func(n) = n == 2);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming `predicate` has O(1) costs.
  public func any<T>(stack : Stack<T>, predicate : T -> Bool) : Bool {
    for (element in values(stack)) {
      if (predicate(element)) {
        return true
      }
    };
    false
  };

  /// Applies the operation to each element in the stack, from top to bottom.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   var text = "";
  ///   Stack.forEach<Nat>(stack, func(n) = text #= Nat.toText(n));
  ///   assert text == "123";
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `operation` has O(1) costs.
  public func forEach<T>(stack : Stack<T>, operation : T -> ()) {
    for (element in values(stack)) {
      operation(element)
    }
  };

  /// Creates a new stack by applying the projection function to each element.
  /// Maintains the original order of elements.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   let doubled = Stack.map<Nat, Nat>(stack, func(n) { 2 * n });
  ///   assert Stack.get(doubled, 0) == ?2;
  ///   assert Stack.get(doubled, 1) == ?4;
  ///   assert Stack.get(doubled, 2) == ?6;
  ///   assert Stack.get(doubled, 3) == null;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `project` has O(1) costs.
  public func map<T, U>(stack : Stack<T>, project : T -> U) : Stack<U> {
    let result = empty<U>();
    for (element in values(stack)) {
      push(result, project(element))
    };
    reverse(result);
    result
  };

  /// Creates a new stack containing only elements that satisfy the predicate.
  /// Maintains the relative order of elements.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 4);
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   let evens = Stack.filter<Nat>(stack, func(n) { n % 2 == 0 });
  ///   assert Stack.pop(evens) == ?2;
  ///   assert Stack.pop(evens) == ?4;
  ///   assert Stack.pop(evens) == null;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming `predicate` has O(1) costs.
  public func filter<T>(stack : Stack<T>, predicate : T -> Bool) : Stack<T> {
    let result = empty<T>();
    for (element in values(stack)) {
      if (predicate(element)) {
        push(result, element)
      }
    };
    reverse(result);
    result
  };

  /// Creates a new stack by applying the projection function to each element
  /// and keeping only the successful results (where project returns ?value).
  /// Maintains the relative order of elements.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  ///
  /// persistent actor {
  ///   let stack = Stack.empty<Nat>();
  ///   Stack.push(stack, 4);
  ///   Stack.push(stack, 3);
  ///   Stack.push(stack, 2);
  ///   Stack.push(stack, 1);
  ///   let evenDoubled = Stack.filterMap<Nat, Nat>(stack, func(n) {
  ///     if (n % 2 == 0) {
  ///       ?(n * 2)
  ///     } else {
  ///       null
  ///     }
  ///   });
  ///   assert Stack.pop(evenDoubled) == ?4;
  ///   assert Stack.pop(evenDoubled) == ?8;
  ///   assert Stack.pop(evenDoubled) == null;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `project` has O(1) costs.
  public func filterMap<T, U>(stack : Stack<T>, project : T -> ?U) : Stack<U> {
    let result = empty<U>();
    for (element in values(stack)) {
      switch (project(element)) {
        case null {};
        case (?newElement) {
          push(result, newElement)
        }
      }
    };
    reverse(result);
    result
  };

  /// Compares two stacks for equality using the provided equality function.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let stack1 = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   let stack2 = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   assert Stack.equal(stack1, stack2, Nat.equal);
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `equal` has O(1) costs.
  public func equal<T>(stack1 : Stack<T>, stack2 : Stack<T>, equal : (T, T) -> Bool) : Bool {
    if (size(stack1) != size(stack2)) {
      return false
    };
    let iterator1 = values(stack1);
    let iterator2 = values(stack2);
    loop {
      let element1 = iterator1.next();
      let element2 = iterator2.next();
      switch (element1, element2) {
        case (null, null) {
          return true
        };
        case (?element1, ?element2) {
          if (not equal(element1, element2)) {
            return false
          }
        };
        case _ { return false }
      }
    }
  };

  /// Creates a new stack from an iterator.
  /// Elements are pushed in iteration order. Which means that the last element
  /// of the iterator will be the first element on top of the stack.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let stack = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   assert Iter.toArray(Stack.values(stack)) == [1, 2, 3];
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of iterated elements.
  public func fromIter<T>(iter : Types.Iter<T>) : Stack<T> {
    let stack = empty<T>();
    for (element in iter) {
      push(stack, element)
    };
    stack
  };

  /// Converts the stack to its string representation using the provided
  /// element formatting function.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let stack = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   assert Stack.toText(stack, Nat.toText) == "Stack[1, 2, 3]";
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(n)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `format` has O(1) costs.
  public func toText<T>(stack : Stack<T>, format : T -> Text) : Text {
    var text = "Stack[";
    var sep = "";
    for (element in values(stack)) {
      text #= sep # format(element);
      sep := ", "
    };
    text #= "]";
    text
  };

  /// Compares two stacks lexicographically using the provided comparison function.
  ///
  /// Example:
  /// ```motoko
  /// import Stack "mo:core/Stack";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let stack1 = Stack.fromIter<Nat>([2, 1].values());
  ///   let stack2 = Stack.fromIter<Nat>([3, 2, 1].values());
  ///   assert Stack.compare(stack1, stack2, Nat.compare) == #less;
  /// }
  /// ```
  ///
  /// Runtime: O(n)
  /// Space: O(1)
  /// where `n` denotes the number of elements stored on the stack and
  /// assuming that `compare` has O(1) costs.
  public func compare<T>(stack1 : Stack<T>, stack2 : Stack<T>, compare : (T, T) -> Order.Order) : Order.Order {
    let iterator1 = values(stack1);
    let iterator2 = values(stack2);
    loop {
      switch (iterator1.next(), iterator2.next()) {
        case (null, null) return #equal;
        case (null, _) return #less;
        case (_, null) return #greater;
        case (?element1, ?element2) {
          let comparison = compare(element1, element2);
          if (comparison != #equal) {
            return comparison
          }
        }
      }
    }
  }
}
