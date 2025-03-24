func foo<T, K <: T -> ()>(k : K, v : T) = k v;

foo<Nat, Int -> ()>(func(i : Int) {}, 1);
