import Map "./Map";

persistent actor {
  // Passes, which seems suspicious? The `comp` name does not make it to the actor scope
  let compare = func comp(t1 : Text, t2 : Text) : Int {
    if (t1 > t2) 1 else if (t1 < t2) -1 else 0
  };

  var users : Map.Map<Text, Nat> = Map.Map<Text, Nat>(compare);
}
