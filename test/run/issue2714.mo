type A = { #y : A };
// func bar(x: A): Text { debug_show(x); };
func foo(blob: ?A) : () { ignore (debug_show(blob)); };
foo(null);
