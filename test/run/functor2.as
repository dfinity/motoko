let x : Int = 1;

class Weird<T> () {
  type t<U> = ?(T,<V>()-> (t<U>,t<V>));
};

func WeirdInt() : Weird<Int> = Weird<Int>();

func WeirdBool() : Weird<Bool> = Weird<Bool>();
