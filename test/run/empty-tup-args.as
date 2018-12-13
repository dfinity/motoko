func f() {};
func g(()) {};
func h(u:()) {};
let _ = f : () -> ();
let _ = g : () -> ();
let _ = h : (()) -> ();
f();
g();
h();
