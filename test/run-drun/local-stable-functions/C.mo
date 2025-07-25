import Map "./Map";

persistent actor {
  // I'd assumed this wouldn't work as we're passing an anonymous (non-stable) function
  // Named functions in argument position (unnamed context) should not be considered stable
  var users : Map.Map<Text, Nat> = Map.Map(func l(t1 : Text, t2 : Text) : Int {
    if (t1 > t2) 1 else if (t1 < t2) -1 else 0
  });
}
