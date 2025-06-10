module {
  public type T = Nat;
  public type Result<Ok, Err> = {
    #ok : Ok;
    #err : Err;
  };
}
