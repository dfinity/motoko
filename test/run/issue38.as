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

func m((a:Int, b:Bool)) {};
func m1((a:Int, b:Bool,)) {};
let ms : [((Int, Bool)) -> ()] = [m, m1];

func n(a:Int, b:Bool) {};
func n1(a:Int, b:Bool,) {};

let ns : [(Int, Bool) -> ()] = [n, n1];

func o((a:Int,)) {};
let os : [((Int,)) -> ()] = [o];

func p((),) {};
let ps : [((),) -> ()] = [p];

/* the above is a curious case, failing in wasm:

issue38: [tc] [run] [run-low] [run-ir] [wasm]
--- issue38.wasm.stderr (expected)
+++ issue38.wasm.stderr (actual)
@@ -0,0 +1,5 @@
+Ill-typed intermediate code (use -v to see dumped IR):
+issue38.as:44.1-44.15: IR type error, subtype violation:
+  (((),)) -> ()
+  (()) -> ()
+
*/
