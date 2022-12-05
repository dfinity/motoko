/// Stack collection (LIFO discipline).
///
/// Minimal LIFO (last in first out) implementation, as a class.
/// See library `Deque` for mixed LIFO/FIFO behavior.
///
import List "List";

module {

  public class Stack<T>() {

    var stack : List.List<T> = List.nil<T>();

    /// Push an element on the top of the stack.
    public func push(x:T) {
      stack := ?(x, stack)
    };

    /// True when the stack is empty.
    public func isEmpty() : Bool {
      List.isNil<T>(stack)
    };

    /// Return and retain the top element, or return null.
    public func peek() : ?T {
      switch stack {
        case null { null };
        case (?(h, t)) { ?h };
      }
    };

    /// Remove and return the top element, or return null.
    public func pop() : ?T {
      switch stack {
        case null { null };
        case (?(h, t)) { stack := t; ?h };
      }
    };
  };
}
