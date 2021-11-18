type V = {#vari; #ant};

let v0 = #ant; // inferred as a singleton type
var w0 = v0;

let v1 : V = #ant;
var w1 = v1;

// CHECK: (func $bar (type 11) (param $clos i32))
func bar() {
    ignore (v0 == w0) // trivial equality for singletons
};

// CHECK: (func $foo
// CHECK: call $@eq<v_ant:u_vari:u_>
// CHECK: call $@eq<v_ant:u_vari:u_>
func foo() {
    assert v1 == w1;
    assert #vari != w1
};

type Y = { #good; #bad : Text };
let y : Y = #good;
var z : Y = y;

// CHECK: (func $quux
// CHECK: call $@eq<v_bad:t_good:u_>
func quux() {
    assert y == z;
    assert z != #bad "Dunno!"
};

quux();
foo();
bar()
