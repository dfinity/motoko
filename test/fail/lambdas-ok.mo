func mapMono<T>(ar : [T], _f : T -> T) : [T] = ar;
func mapMonoTuple<T>(ar : [T], _t : (T -> T, T -> T)) : [T] = ar;
func forEach<T>(ar : [T], _f : T -> ()) {};
func forEachTuple<T>(ar : [T], _t : (T -> (), T -> ())) {};

let ar = [1, 2, 3];
let _ = mapMono(ar, func x = x + 1);
let _ = mapMonoTuple(ar, (func x = x + 1, func x = x + 1));
let _ = forEach(ar, func x {});
let _ = forEachTuple(ar, (func x = (), func x = ()));