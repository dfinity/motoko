
func h<A,B>(f: A -> B) { };
type T = (Int, Int);
func f(x: (Nat, Nat)) : (Int, Int) { x };
func g(x: (Nat, Nat)) : T { x };
h<(Nat,Nat),(Int,Int)>(f); // reject
h<(Nat,Nat),(Int,Int)>(g); // accept

