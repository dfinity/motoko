module Collections = {
  module List = {
    type List<T> = ?(T, List<T>);
    func nil<T>() : List<T> = null;
    func cons<T>(x : T, l : List<T>) : List<T> = ?(x, l);
  };
};

let List = Collections.List;

type Stack = List.List<Int>;

let empty : Stack = List.nil<Int>();
func push(x : Int, s : Stack) : Stack = List.cons<Int>(x, s);

