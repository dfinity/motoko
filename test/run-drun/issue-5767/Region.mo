
import Prim "mo:⛔";

module {

  public type Region = Prim.Types.Region;

  public let new : () -> Region = Prim.regionNew;

  public func grow(self : Region, newPages : Nat64) : (oldPages : Nat64) = Prim.regionGrow(self, newPages);

  public func loadNat64(self : Region, offset : Nat64) : Nat64 = Prim.regionLoadNat64(self, offset);

  public func loadNat64Fn(r : Region, offset : Nat64) : Nat64 = Prim.regionLoadNat64(r, offset);


}
