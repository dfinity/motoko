func f1<T>(x : T) : T { x };

() |> f1 ();  // no hole, syntax error
