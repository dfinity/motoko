module M {
  public func withImplicit(_ : Int, _ : (implicit : Text), _ : Nat) {};
  public func withoutImplicit(_ : Int, _ : Text, _ : Nat) {};
};
let arg = (1 : Int, "abc", 2);

func singleArgAsMany1() {
  let _ = M.withoutImplicit(arg);
  let _ = M.withImplicit(arg);
};
func singleArgAsMany2() {
  let _ = M.withoutImplicit arg;
  let _ = M.withImplicit arg;
};
