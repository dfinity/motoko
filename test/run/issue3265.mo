// simplest example of failure
let rec1 : { var myList : Int } = { var myList = (0 : Nat)};

// repro from #3265
type List<X> = ?(X, List<X>);
func nil<X>() : List<X> = null;
func sub(x : List<None>) : List<Nat>{x};
let rec2 : { var myList : List<Nat> } = { var myList = nil<None>() };
