module VarArray {
  public func map<T, R>(_ : [var T], _ : T -> R) : [var R] = [var];
};
func check<T>(t1 : T, t2 : T) : [T] = [t1, t2]; // used to check type equality

let testArray = [var 1, 2, 3, 4, 5];

let result1 = VarArray.map<Nat, Nat>(testArray, func x = x * 2);

let result2 = VarArray.map<_, Nat>(testArray, func x = x * 2);

let result3 : [var Nat] = VarArray.map<Nat, _>(testArray, func x = x * 2);

let result4 : [var Nat] = VarArray.map<_, _>(testArray, func x = x * 2);

let result5 = VarArray.map<_, Text>(testArray, func x = debug_show(x));

let result6 = VarArray.map<_, Bool>(testArray, func x = x > 2);

let nats : [var Nat] = [var];
let bools : [var Bool] = [var];
let texts : [var Text] = [var];

ignore check(result1, nats);
ignore check(result2, nats); 
ignore check(result3, nats);
ignore check(result4, nats);
ignore check(result5, texts);
ignore check(result6, bools);
