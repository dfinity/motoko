type A = actor { foo : ()->(); bar : () -> () };
assert((actor "aaaaa-aa" : A) == (actor "aaaaa-aa" : A));
assert((actor "aaaaa-aa" : A) != (actor "psokg-ww6vw-7o6" : A));
