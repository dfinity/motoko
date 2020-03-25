/**
[#mod-Nat]
= `Nat` -- Natural numbers

Most operations on natural numbers (e.g. addition) are available as built-in operators (`1 + 1`).
This module provides conversion functions.

The conversions `toNat*` will trap if the number is out of bounds; the conversions `toWord*` will wrap-around.

*/

import Prim "mo:prim";
import Int "Int";
import Prelude "Prelude";

module {

  public let toText : Nat -> Text = Int.toText;

  public let fromWord8  : Word8  -> Nat = Prim.word8ToNat;
  public let fromWord16 : Word16 -> Nat = Prim.word16ToNat;
  public let fromWord32 : Word32 -> Nat = Prim.word32ToNat;
  public let fromWord64 : Word64 -> Nat = Prim.word64ToNat;

  public let toWord8    : Nat -> Word8  = Prim.natToWord8;
  public let toWord16   : Nat -> Word16 = Prim.natToWord16;
  public let toWord32   : Nat -> Word32 = Prim.natToWord32;
  public let toWord64   : Nat -> Word64 = Prim.natToWord64;

  public let fromNat8  : Nat8  -> Nat = Prim.nat8ToNat;
  public let fromNat16 : Nat16 -> Nat = Prim.nat16ToNat;
  public let fromNat32 : Nat32 -> Nat = Prim.nat32ToNat;
  public let fromNat64 : Nat64 -> Nat = Prim.nat64ToNat;

  public let toNat8    : Nat -> Nat8  = Prim.natToNat8;
  public let toNat16   : Nat -> Nat16 = Prim.natToNat16;
  public let toNat32   : Nat -> Nat32 = Prim.natToNat32;
  public let toNat64   : Nat -> Nat64 = Prim.natToNat64;

  // Remove this?
  public func add(x : Nat, y : Nat) : Nat {
    x + y;
  };

  public let min : (Nat, Nat) -> Nat = func(x,y) {
    if (x < y) x else y;
  };

  public let max : (Nat, Nat) -> Nat = func(x,y) {
    if (x < y) y else x;
  };
}
