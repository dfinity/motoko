func foo<T, K <: T -> ()>(k : K, v : T) = k v;


//foo<(Nat, Nat), (Nat, Nat)->()>(func(Int, Nat){}, (3,4));
foo<(Nat, Nat), ((Nat, Nat))->()>(func(Int, Nat){}, (3,4));
//foo<((Nat, Nat)), (Nat, Nat)->()>(func(Int, Nat){}, (3,4));
foo<((Nat, Nat)), ((Nat, Nat))->()>(func(Int, Nat){}, (3,4));


//foo<(Nat, Nat), (Nat, Nat)->()>(func((Int, Nat)){}, (3,4));
foo<(Nat, Nat), ((Nat, Nat))->()>(func((Int, Nat)){}, (3,4));
//foo<((Nat, Nat)), (Nat, Nat)->()>(func((Int, Nat)){}, (3,4));
foo<((Nat, Nat)), ((Nat, Nat))->()>(func((Int, Nat)){}, (3,4));
