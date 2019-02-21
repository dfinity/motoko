func f() {};
func g(()) {};
func g1((())) {}; // only top-level parenthesis signifies arity
func h(u:()) {};
func h1((u:())) {}; // dito, intermediate parentheses are redundant

let _ : [() -> ()] = [f];
let gh : [(()) -> ()] = [g, g1, h, h1];

f();
g(); // accepted, because "At calls there is an implicit coercion
     //                    between tuples and n-ary arguments"
h(); // dito
gh[0](); // dito
gh[1](); // dito

// Correct:
g(());
h(());
gh[0](());
gh[1](());

func j(a:Int) {};
func k(a:(Int)) {};
func l(a:((Int))) {};

let jkl : [Int -> ()] = [j, k, l];

j(5);
k(8);
l(13);
jkl[1](21);

func m((a:Int, b:Bool)) {};
func m1((a:Int, b:Bool,)) {};
let ms : [((Int, Bool)) -> ()] = [m, m1];

func n(a:Int, b:Bool) {};
func n1(a:Int, b:Bool,) {};

let ns : [(Int, Bool) -> ()] = [n, n1];

func o((a:Int,)) {};
let os : [((Int,)) -> ()] = [o];

/* TODO
func q((?(a:Int),)) {};
func q1((?((a:Int)),)) {};
func q2(((?((a:Int)),))) {};
let qs : [((?Int,)) -> ()] = [q, q1, q2]
*/


// class tests, these have the same argument regime
class F() {};
class G(()) {};
class H(a:()) {};
class G1((())) {};
class H1((u:())) {};
let _ : [() -> F] = [F];
let _ : [(()) -> G] = [G];
let _ : [(()) -> G1] = [G1];
let _ : [(()) -> H] = [H];
let _ : [(()) -> H1] = [H1];

func annih<A,B>(f : A -> B) : (A -> ()) { func (a : A) { ignore (f a) } };
//func annih<A,B>(f : A -> B) : (A -> ()) { func (a : A) { let _ = f a; } };
// fishy: let _ : [() -> ()] = [annih<(),F> F];
let _ : [(()) -> ()] = [annih<(),G> G, annih<(),G1> G1, annih<(),H> H, annih<(),H1> H1];
