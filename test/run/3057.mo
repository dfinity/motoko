module {
  public class Id<A>(x : A) {
    public func map<B>(f : A -> B) : Id<B> = Id<B>(f x)
  }
}

