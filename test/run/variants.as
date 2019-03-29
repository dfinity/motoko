type Tree<A> = {#leaf : A; #branch : (Tree<A>, Tree<A>)};

func size<A>(t : Tree<A>) : Nat {
  switch t {
  case (#leaf _) 1;
  case (#branch(t1, t2)) { 1 + size<A>(t1) + size<A>(t2) };
  }
};

let t1 : Tree<Int> = #branch(#leaf 1, #leaf (-1));
let t2 = #leaf ""; // infers type {#leaf : Text} which is a subtype of Tree<Text>

printInt(size<Int>(t1));
printInt(size<Text>(t2));
