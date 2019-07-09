module {
  private type Params = {
    dispose : () -> ();
  };

  class Type(params : Params) {
    let dispose = params.dispose;
  };

  func init(params : Params) : Type {
    Type(params);
  };

  // NOTE: we wrap this in a function because otherwise the compiler complains
  // with "type error, non-static expression in library or module".
  func empty() : Type {
    Type(new {
      dispose = func () {};
    });
  };
};
