func f2<T1, T2>(x1 : T1, x2 : T2) : (T1, T2) { (x1, x2) };

let (#A, #B) = #A |> f2(#A, #B); //no hole, syntax error


