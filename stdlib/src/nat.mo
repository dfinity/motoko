import Prim "mo:prim";
import Int "int.mo";
import Prelude "prelude.mo";

module {
  public func add(x : Nat, y : Nat) : Nat {
    x + y;
  };

  public let toText : Nat -> Text = Int.toText;

  public let fromWord8  = Prim.word8ToNat;
  public let fromWord16 = Prim.word16ToNat;
  public let fromWord32 = Prim.word32ToNat;
  public let fromWord64 = Prim.word64ToNat;
  public let toWord8    = Prim.natToWord8;
  public let toWord16   = Prim.natToWord16;
  public let toWord32   = Prim.natToWord32;
  public let toWord64   = Prim.natToWord64;

  public let fromNat8  = Prim.nat8ToNat;
  public let fromNat16 = Prim.nat16ToNat;
  public let fromNat32 = Prim.nat32ToNat;
  public let fromNat64 = Prim.nat64ToNat;
  public let toNat8    = Prim.natToNat8;
  public let toNat16   = Prim.natToNat16;
  public let toNat32   = Prim.natToNat32;
  public let toNat64   = Prim.natToNat64;

}
