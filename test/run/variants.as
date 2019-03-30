type Tree<A> = {#leaf : A; #branch : (Tree<A>, Tree<A>)};

func size<A>(t : Tree<A>) : Nat {
  switch t {
  case (#leaf _) 1;
  case (#branch(t1, t2)) { 1 + size<A>(t1) + size<A>(t2) };
  }
};

let tt1 : Tree<Int> = #branch(#leaf 1, #leaf (-1));
let tt2 = #leaf ""; // infers type {#leaf : Text} which is a subtype of Tree<Text>

printInt(size<Int>(tt1)); print "\n";
printInt(size<Text>(tt2)); print "\n";


// subtyping

type Super = {#c : Int; #b : Char; #a : Nat};
type Sub = {#c : Nat; #a : Nat};

let ts1 : Sub = #c 25;
func ts2(v : Super) { ignore v };

let ts3 = ts2 ts1;
