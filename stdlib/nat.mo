import Int "int.mo";
import Prelude "prelude.mo";

module {
  public func add(x : Nat, y : Nat) : Nat {
    x + y;
  };

  public func toText(x : Nat) : Text {
    Int.toText(x);
  };

  public func fromWord8 (x : Word8):  Nat = word8ToNat x;
  public func fromWord16(x : Word16): Nat = word16ToNat x;
  public func fromWord32(x : Word32): Nat = word32ToNat x;
  public func fromWord64(x : Word64): Nat = word64ToNat x;
  public func toWord8 (x : Nat) : Word8   = natToWord8  x;
  public func toWord16(x : Nat) : Word16  = natToWord16 x;
  public func toWord32(x : Nat) : Word32  = natToWord32 x;
  public func toWord64(x : Nat) : Word64  = natToWord64 x;

  public func fromNat8 (x : Nat8):  Nat = nat8ToNat x;
  public func fromNat16(x : Nat16): Nat = nat16ToNat x;
  public func fromNat32(x : Nat32): Nat = nat32ToNat x;
  public func fromNat64(x : Nat64): Nat = nat64ToNat x;
  public func toNat8 (x : Nat) : Nat8   = natToNat8  x;
  public func toNat16(x : Nat) : Nat16  = natToNat16 x;
  public func toNat32(x : Nat) : Nat32  = natToNat32 x;
  public func toNat64(x : Nat) : Nat64  = natToNat64 x;

}
