// works
func call1<A <: Shared>(f : shared () -> ()) : () { f(); };
func call2<B <: Shared>(f : shared () -> async B) : async B { await f(); };
// does not work
// func call3<C <: Shared>(f : shared () -> C) : C { f (); };

let a = actor { get42() : async Nat = async { 42 }; };
let _ = async { printInt(await (call2<Nat>(a.get42))); };


//func call3<B <: Shared>(f : shared () -> async B) : async B { f(); };

func call3<B <: Shared>(f : shared () -> async B) : async B = f() ;

// illegal:
// shared func call4<B <: Shared>(f : shared () -> async B) : async B = f() ;

shared func call4<B <: Shared>(f : shared () -> async B) : async B = async await f() ;