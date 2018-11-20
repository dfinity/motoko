func call3<B <: Shared>(f : shared () -> async B) : async B { f(); };
