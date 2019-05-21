module {

private import List = "ListLib.as"; // private, so we don't re-export List

type Stack = List.List<Int>;

func push(x : Int, s : Stack) : Stack = List.cons<Int>(x, s);

func empty():Stack = List.nil<Int>();

}
