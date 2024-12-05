/// Iterators


module {

  public class Iter<T>({next = n : () -> ?T}) = this {

    public let next = n;

    public func map<R>(f : T -> R) : Iter<R> {
       loop {}
    };
  }

}
