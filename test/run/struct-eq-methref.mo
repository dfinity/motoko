type A = actor { foo : ()->(); bar : () -> () };
assert((actor "aaaaa-aa" : A).foo == (actor "aaaaa-aa" : A).foo);
assert((actor "aaaaa-aa" : A).foo != (actor "aaaaa-aa" : A).bar);
assert((actor "aaaaa-aa" : A).foo != (actor "psokg-ww6vw-7o6" : A).foo);

// The interpreter can’t make sense of actor method referenes
//SKIP run
//SKIP run-ir
//SKIP run-low
