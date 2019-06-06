// stackoverflows due, I believe, to polymorphic recursion in t

class Weird<T> () {
  type t<U> = ?(T,<V>()-> (t<U>,t<V>));
};

func WeirdInt() : Weird<Int> = Weird<Int>();

func WeirdBool() : Weird<Bool> = Weird<Bool>();
