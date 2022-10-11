func mix<A <: { a : Int }, B <: { b : Char }>(a : A, b : B) : A =
    { a and b with c = "Nope" };
