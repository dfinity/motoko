

((func <T>(v : T) : () = ()) : <T>T -> (())) ((3,4));
((func <T>(v : T) : (()) = ()) : <T>T -> (())) ((3,4));
((func <T>(v : T) : (()) = ()) : <T>T -> (())) ((3,4));
((func <T>(v : T) : (()) = ()) : <T>T -> ()) ((3,4));
((func <T>(v : T) : (()) = ()) : <T>T -> ()) (3,4);
// TODO: test all combinations


// TODO: debug_show the argument to demonstrate it is intactly passed



func foo<T, K <: T -> ()>(k : K, v : T) = k v;


//foo<(Nat, Nat), (Nat, Nat)->()>(func(Int, Nat){}, (3,4));
foo<(Nat, Nat), ((Nat, Nat))->()>(func(Int, Nat){}, (3,4));
//foo<((Nat, Nat)), (Nat, Nat)->()>(func(Int, Nat){}, (3,4));
foo<((Nat, Nat)), ((Nat, Nat))->()>(func(Int, Nat){}, (3,4));


//foo<(Nat, Nat), (Nat, Nat)->()>(func((Int, Nat)){}, (3,4));
foo<(Nat, Nat), ((Nat, Nat))->()>(func((Int, Nat)){}, (3,4));
//foo<((Nat, Nat)), (Nat, Nat)->()>(func((Int, Nat)){}, (3,4));
foo<((Nat, Nat)), ((Nat, Nat))->()>(func((Int, Nat)){}, (3,4));
