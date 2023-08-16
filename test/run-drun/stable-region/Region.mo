import Prim "mo:⛔";

module {

  public let new = Prim.regionNew;
  public let id = Prim.regionId;

  public let size = Prim.regionSize;
  public let grow = Prim.regionGrow;

  public let loadNat32 = Prim.regionLoadNat32;
  public let storeNat32 = Prim.regionStoreNat32;

  public let loadNat8 = Prim.regionLoadNat8;
  public let storeNat8 = Prim.regionStoreNat8;

  public let loadNat16 = Prim.regionLoadNat16;
  public let storeNat16 = Prim.regionStoreNat16;

  public let loadNat64 = Prim.regionLoadNat64;
  public let storeNat64 = Prim.regionStoreNat64;

  public let loadFloat = Prim.regionLoadFloat;
  public let storeFloat= Prim.regionStoreFloat;

  public let loadInt32 = Prim.regionLoadInt32;
  public let storeInt32 = Prim.regionStoreInt32;

  public let loadInt8 = Prim.regionLoadInt8;
  public let storeInt8 = Prim.regionStoreInt8;

  public let loadInt16 = Prim.regionLoadInt16;
  public let storeInt16 = Prim.regionStoreInt16;

  public let loadInt64 = Prim.regionLoadInt64;
  public let storeInt64 = Prim.regionStoreInt64;

  public let loadBlob = Prim.regionLoadBlob;
  public let storeBlob = Prim.regionStoreBlob;

}
