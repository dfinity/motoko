/**
[#mod-none]
= `none` -- The absent value
*/

module {
  public func absurd<A>(x : None) : A {
    switch (x) {};
  };
}
