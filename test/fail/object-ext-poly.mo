func mix<A <: { a : Int }, B <: { b : Char }>(a : A, b : B) : A /*and B*/ =
    { a and b with c = "Nope" }
