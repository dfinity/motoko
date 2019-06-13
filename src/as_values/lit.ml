open As_types

type lit =
  | NullLit
  | BoolLit of bool
  | NatLit of Value.Nat.t
  | IntLit of Value.Int.t
  | Word8Lit of Value.Word8.t
  | Word16Lit of Value.Word16.t
  | Word32Lit of Value.Word32.t
  | Word64Lit of Value.Word64.t
  | FloatLit of Value.Float.t
  | CharLit of Value.unicode
  | TextLit of string
  | PreLit of string * Type.prim

