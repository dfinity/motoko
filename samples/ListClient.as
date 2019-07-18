module {

import List = "ListLib.as"; // private, so we don't re-export List

public type Stack = List.List<Int>;

public func push(x : Int, s : Stack) : Stack = List.cons<Int>(x, s);

public func empty() : Stack = List.nil<Int>();

}
