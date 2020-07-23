// This test just checks that we can use
// the `prim` syntax in our tests
import Prim "mo:prim";
assert(Prim.rts_version() == ((prim "rts_version" : () -> Text) ()));

