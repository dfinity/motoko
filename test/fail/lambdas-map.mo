//   let _ = map**(ar, func x = x + 1)**;
//   let _ = map(ar, **func x = x + 1**);

//   Nat  <:  I  <:  Any

//   None  <:  O  <:  Any
// I := Nat
// O := None
// test/fail/lambdas-map.mo:5.28-5.29: type error [M0096], expression of type
//   Nat
// cannot produce expected type
//   None
// test/fail/lambdas-map.mo:5.32-5.33: type error [M0050], literal of type
//   Nat
// does not have expected type
//   None
func map<I, O>(_ar : [I], _f : I -> O) : [O] = [];

func _useMap() {
  let ar : [Nat] = [1, 2, 3];
  let _ = map(ar, func x = x + 1);
};
