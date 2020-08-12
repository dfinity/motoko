type T = { #exp : Nat8 };

func foo() : T = #exp 42; // this should pass

let why : Nat = 42;
func bar() : T = #exp why; // this should fail
