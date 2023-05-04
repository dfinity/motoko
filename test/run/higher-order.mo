func callee(i : Int, c : Char) {};
func poly_callee<T <: Int>(i : T, c : Char) {};

func caller(pair : (Nat, Char), pairs : [(Int, Char)], f : (Int, Char) -> ()) {
  f pair;
  f (pair.0, pair.1);
  callee pair;
  callee (pair.0, pair.1);

  f (pairs[0]);
  f (pairs[0].0, pairs[0].1);
  callee (pairs[0]);
  callee (pairs[0].0, pairs[0].1);
};

caller((42, 'H'), [(25, 'G')], callee);
caller((42, 'H'), [(25, 'G')], func (42 or 25, 'G' or 'H') {});
caller((42, 'H'), [(25, 'G')], poly_callee);


func poly_caller<A>(arg : A -> Text) {};
poly_caller(func(_ : Int, _ : Char) : Text = "YO");
