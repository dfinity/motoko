do { type T = (implicit x : Nat); };

do {
  func id<T>(x : T) : T { x };
  id<(implicit x : Nat)>(1);
};

do {
  type Id<T> = T;
  type U = Id<(implicit : Nat)>;
};

do {
 type R = (implicit : Nat);
};


do {
 type F = () -> (implicit : Nat);
};

do {
 type Ok1 = (implicit x : Nat) -> ();
 type Ok2 = (implicit x : Nat, implicit x : Nat) -> ();
};
