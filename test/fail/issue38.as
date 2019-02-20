func f() {};
func g(()) {};
func h(u:()) {};

let _ : [() -> ()] = [f];
let gh : [(()) -> ()] = [g, h];

f();
g(); // why is this accepted?
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

