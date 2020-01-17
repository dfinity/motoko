import Prim "mo:prim";
import Int "int";
import Prelude "prelude";

module {
  public func add(x : Nat, y : Nat) : Nat {
    x + y;
  };

  public func toText(x : Nat) : Text {
    Int.toText(x);
  };

  public func fromWord8 (x : Word8):  Nat = Prim.word8ToNat x;
  public func fromWord16(x : Word16): Nat = Prim.word16ToNat x;
  public func fromWord32(x : Word32): Nat = Prim.word32ToNat x;
  public func fromWord64(x : Word64): Nat = Prim.word64ToNat x;
  public func toWord8 (x : Nat) : Word8   = Prim.natToWord8  x;
  public func toWord16(x : Nat) : Word16  = Prim.natToWord16 x;
  public func toWord32(x : Nat) : Word32  = Prim.natToWord32 x;
  public func toWord64(x : Nat) : Word64  = Prim.natToWord64 x;

  public func fromNat8 (x : Nat8):  Nat = Prim.nat8ToNat x;
  public func fromNat16(x : Nat16): Nat = Prim.nat16ToNat x;
  public func fromNat32(x : Nat32): Nat = Prim.nat32ToNat x;
  public func fromNat64(x : Nat64): Nat = Prim.nat64ToNat x;
  public func toNat8 (x : Nat) : Nat8   = Prim.natToNat8  x;
  public func toNat16(x : Nat) : Nat16  = Prim.natToNat16 x;
  public func toNat32(x : Nat) : Nat32  = Prim.natToNat32 x;
  public func toNat64(x : Nat) : Nat64  = Prim.natToNat64 x;

}
