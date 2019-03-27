type Tree<A> = {#leaf : A; #branch : (Tree<A>, Tree<A>)};

func size<A>(t : Tree<A>) : Nat {
  switch t {
  case (#leaf _) 0;
  case (#tree(t1, t2)) { 1 + size(t1) + size(t2) };
  }
}

let t1 : Tree<Int> = #branch(#leaf 1, #leaf (-1));
let t2 = #leaf ""; // infers type {#leaf : Text} which is a subtype of Tree<Text>

printInt(size(t1));
printInt(size(t2));
