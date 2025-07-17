func foo<T, I>(t : T, f : I -> T) {};
func useFoo() {
  foo(1, func x = x + 1);
};