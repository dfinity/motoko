type t<T> = ?(T, t<T>);
func nil<T>() : t<T> = null;
func cons<T>(x : T, l : t<T>) : t<T> = ?(x, l);
