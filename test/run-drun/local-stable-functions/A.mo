import Map "Map";

persistent actor {

  func compare(t1 : Text, t2 : Text) : Int {
    if (t1 > t2) 1 else if (t1 < t2) -1 else 0
  };

  // Fails with a pretty obtuse error message. That's unfortunate as this used to be fine?
  // NOTE(Claudio): Bi-Matching rules need to be adjusted for stable function subtyping
  // var users : Map.Map<Text, Nat> = Map.Map(compare);

  // actor-local-func.mo:9.36-9.52: type error [M0098], cannot implicitly instantiate function of type
  //   <K, V>(compare : (K, K) -> Int) -> Map<K, V>
  // to argument of type
  //   stable (t1 : Text, t2 : Text) -> Int
  // to produce result of type
  //   {insert : stable (key : Text, value : Nat) -> ?Nat}
  // because no instantiation of K__2, V__2 makes
  //   stable (t1 : Text, t2 : Text) -> Int  <:  (compare : (K__2, K__2) -> Int)
  // and
  //   Map<K__2, V__2>  <:  {insert : stable (key : Text, value : Nat) -> ?Nat}
  //

  var users : Map.Map<Text, Nat> = Map.Map<Text, Nat>(compare);
}
