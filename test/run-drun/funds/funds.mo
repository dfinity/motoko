import Prim "mo:prim";

module {

  public type Unit = Prim.Unit;

  public let balance : (u : Unit) -> Nat64 = Prim.fundsBalance;

  public let available : (u : Unit) -> Nat64 = Prim.fundsAvailable;

  public let accept : (u : Unit, amount : Nat64) -> () = Prim.fundsAccept;

  public let transfer : (u : Unit, amount : Nat64) -> () = Prim.fundsTransfer;

  public let refunded : (u: Unit) -> Nat64 = Prim.fundsRefunded;

}
