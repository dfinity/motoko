import Prim "mo:⛔";

actor {
    let ?x = null else { Prim.trap "x was null" };
};
