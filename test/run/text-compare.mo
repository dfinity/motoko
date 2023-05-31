import Prim "mo:⛔";


assert(Prim.textCompare("", "") == 0);
assert(Prim.blobCompare("", "") == 0);
assert(Prim.textCompare("a", "a") == 0);
assert(Prim.blobCompare("a", "a") == 0);
assert(Prim.textCompare("a", "") == 1);
assert(Prim.blobCompare("a", "") == 1);
assert(Prim.textCompare("a", "b") == -1);
assert(Prim.blobCompare("a", "b") == -1);
assert(Prim.textCompare("", "b") == -1);
assert(Prim.blobCompare("", "b") == -1);

