module Collections = {
  module List = {
    type t<T> = ?(T, t<T>);
    func nil<T>() : t<T> =  null;
    func cons<T>(x : T, l : t<T>) : t<T> =  ?(x, l);
  };
};

type Stack = Collections.List.t<Int>;

func push(x : Int, s : Stack) : Stack = Collections.List.cons<Int>(x, s);

let empty = Collections.List.nil<Int>();
