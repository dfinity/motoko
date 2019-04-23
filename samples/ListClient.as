let List = import "ListLib.as"; // todo: make local so not re-exported in type!

type Stack = List.List<Int>;

func push(x : Int, s : Stack) : Stack = List.cons<Int>(x, s);

let empty = List.nil<Int>();
