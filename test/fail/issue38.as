func f() {};
func g(()) {};
func h(u:()) {};

let _ : [() -> ()] = [f, g, h];

f();
g();
h();