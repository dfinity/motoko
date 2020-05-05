type List<A> = {#nil; #cons : (A, List<A>)};

func show_NatList(x : List<Nat>) : Text = debug_show x;

show_NatList(#cons (0, #cons (1, #nil)));

