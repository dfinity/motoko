// this example triggers non-termination in subtyping due to non-regular type recursion in map<R>
module {

  public class Iter<T>({next = n : () -> ?T}) = this {

    public let next = n;

    public func map<R>(f : T -> R) : Iter<R> {
       loop {}
    };
  };

  public class Iter1<T>({next = n : () -> ?T}) = this {

    public let next = n;

    public func map<R>(f : T -> R) : Iter<R> {
       loop {}
    };
  };


  public class Iter2<T>({next = n : () -> ?T}) = this {

    public let next = n;

    public func map<R>(f : T -> R) : Iter2<R> {
       loop {}
    };
  };


  let v : Iter<Char> = Iter("".chars());
  let v1 : Iter1<Char> = v;
  let v2 : Iter2<Char> = v;

}
