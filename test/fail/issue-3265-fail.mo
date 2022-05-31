// simplest example of failure (negative version)
let rec1 : { var x : Nat } = { var x = (0 : Int)}; // reject

// repro from #3265 (negative version)
type List<X> = ?(X, List<X>);
func nil<X>() : List<X> = null;
func sub(x : List<None>) : List<Nat>{x};
let rec2 : { var myList : List<None> } = { var myList = nil<Nat>() }; // reject
