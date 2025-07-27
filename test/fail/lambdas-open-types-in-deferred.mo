func foo<T, I>(t : T, f : I -> T) {};
func useFoo() {
  foo(1, func x = -1);
  // `1` is used to infer `T`, however `I` cannot be inferred, error
};