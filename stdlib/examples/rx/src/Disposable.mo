module {
  type Dispose = () -> ();

  public class Type(dispose_ : Dispose) {
    let dispose = dispose_;
  };

  public func init(dispose : Dispose) : Type {
    Type(dispose);
  };

  // NOTE: we wrap this in a function because otherwise the compiler complains
  // with "type error, non-static expression in library or module".
  public func empty() : Type {
    Type(func () {});
  };
};
