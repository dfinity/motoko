import Prim "mo:⛔";

module {

  public let size = Prim.stableMemorySize;
  public let grow = Prim.stableMemoryGrow;

  public let loadNat32 = Prim.stableMemoryLoadNat32;
  public let storeNat32 = Prim.stableMemoryStoreNat32;

  public let loadNat8 = Prim.stableMemoryLoadNat8;
  public let storeNat8 = Prim.stableMemoryStoreNat8;

  public let loadNat16 = Prim.stableMemoryLoadNat16;
  public let storeNat16 = Prim.stableMemoryStoreNat16;

  public let loadNat64 = Prim.stableMemoryLoadNat64;
  public let storeNat64 = Prim.stableMemoryStoreNat64;

}
