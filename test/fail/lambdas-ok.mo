func mapMono<T>(ar : [T], _f : T -> T) : [T] = ar;
func mapMonoTuple<T>(ar : [T], _t : (T -> T, T -> T)) : [T] = ar;
func filter<T>(array : [T], f : T -> Bool) : [T] = array;
func filterTuple<T>(ar : [T], _t : (T -> Bool, T -> Bool)) : [T] = ar;

let ar = [1, 2, 3];
let _ = mapMono(ar, func x = x + 1);
let _ = mapMonoTuple(ar, (func x = x + 1, func x = x + 1));
let _ = filter(ar, func x = x > 1);
let _ = filterTuple(ar, (func x = x > 1, func x = x > 1));