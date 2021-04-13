import Prim "mo:⛔";

assert (Prim.encodeUtf8 ("FooBär☃") == ("FooBär☃" : Blob));
assert (Prim.encodeUtf8 ("Foo" # "Bär" # "☃") == ("FooBär☃" : Blob));

assert (Prim.decodeUtf8 ("FooBär☃" : Blob) == ?"FooBär☃");
assert (Prim.decodeUtf8 ("\FF" : Blob) == null);
assert (Prim.decodeUtf8 ("\D8\00t d" : Blob) == null);
