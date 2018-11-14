shared func call4<B <: Shared>(f : shared () -> async B) : async B = f() ;

