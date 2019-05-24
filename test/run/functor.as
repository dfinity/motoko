class List<T> () {
  type t = ?(T,t);
  let nil : t = null;
  func cons(h:T,l:t) : t = cons(h,l);
};



func B() : List<Bool> = List<Bool>();
func I() : List<Int> = List<Int>();



