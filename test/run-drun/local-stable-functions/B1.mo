import Map "./Map";

persistent actor {
  // SUS
  func comp(t1 : Text, t2 : Text) : Int {
    if (t2 > t1) 1 else if (t1 < t2) -1 else 0
  };

  var users : Map.Map<Text, Nat> = Map.Map<Text, Nat>(comp);
}
