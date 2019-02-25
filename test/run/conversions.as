// func Nat32toWord32(n : Nat) : Word32 = prim "Nat32toWord32";

func n2w(n : Nat) : ?Word32 { ?(Nat32toWord32 42) };

assert(Nat32toWord32 42 == (42 : Word32))
