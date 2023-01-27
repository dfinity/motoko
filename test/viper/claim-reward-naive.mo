actor ClaimReward {
  var claimed = false;
  var count = 0 : Int;
  
  assert:invariant 
   (not claimed implies count == 0) // initially synchronized
   and (count == 0 or count == 1);  // reward is unique

  private func reward() : () {
    // (send reward)
    count += 1;
    assert:return count == (old count) + 1;
  };

  public shared func claim() : async () {
    if (not claimed) {
      reward();
      claimed := true;
    };
  };
}
