class Weird<T> () {
  type t<U> = ?(T,<V>()-> t<U>);
};



func WeirdInt() : Weird<Int> = Weird<Int>();

func WeirdBool() : Weird<Bool> = Weird<Bool>();
