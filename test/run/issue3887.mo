import Prim "mo:⛔";
func f() : () {};
(if (true) f else Prim.trap("Not found"))();
