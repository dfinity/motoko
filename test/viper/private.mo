// @verify

actor ClaimReward {
  var claimed = false;
  var count = 0 : Int;
  
  assert:invariant 
   (not claimed implies count == 0) // initially synchronized
   and (count == 0 or count == 1);  // reward is unique

  private func reward() : () {
    assert:func count == 0;
    // (send reward)
    count += 1;
    assert:system count == 1;
  };
}
