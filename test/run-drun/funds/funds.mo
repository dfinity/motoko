import Prim "mo:prim";

module {

  public type Unit = Prim.Unit;

  public let balance : (u : Unit) -> Nat64 = Prim.fundsBalance;

  public let available : (u : Unit) -> Nat64 = Prim.fundsAvailable;

  public let accept : (u : Unit, amount : Nat64) -> () = Prim.fundsAccept;

  public let add : (u : Unit, amount : Nat64) -> () = Prim.fundsAdd;

  public let refunded : (u: Unit) -> Nat64 = Prim.fundsRefunded;

  public func dev_set_funds(a : actor {}, cyles : Nat, icpt : Nat) : async () {
    let ic00 = actor "aaaaa-aa" : actor {
      dev_set_funds :
        { canister_id: Principal; num_cycles : Nat; num_icpt : Nat } -> async ();
    };
    await ic00.dev_set_funds({
      canister_id = Prim.principalOfActor(a);
      num_cycles = cyles;
      num_icpt = icpt})
  }
}
