type List<T> = ?(T, List<T>);
func nil<T>() : List<T> = null;
func cons<T>(x : T, l : List<T>) : List<T> = ?(x, l);

