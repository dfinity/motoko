func app<T>(f: T -> T, x : T) : T { f(x) };

let r : Int = app(func j = -1, 1);
