import Prim "mo:⛔";

module {

  public let size = Prim.stableMemorySize;
  public let grow = Prim.stableMemoryGrow;

  public let loadNat32 = Prim.stableMemoryLoadNat32;
  public let storeNat32 = Prim.stableMemoryStoreNat32;

  public let loadNat8 = Prim.stableMemoryLoadNat8;
  public let storeNat8 = Prim.stableMemoryStoreNat8;

}
