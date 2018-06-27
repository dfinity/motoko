type nat = int
type int8 = int
type int16 = int
type unicode = int32

type width =
  | Width8
  | Width16
  | Width32
  | Width64

type prim =
  | NullT
  | BoolT
  | NatT
  | IntT
  | WordT of width
  | FloatT
  | CharT
  | TextT
  | PreInt of string

type word =
  | Word8 of int8
  | Word16 of int16
  | Word32 of int32
  | Word64 of int64

type lit =
  | NullLit
  | BoolLit of bool
  | NatLit of nat
  | IntLit of int
  | WordLit of word
  | FloatLit of float
  | CharLit of unicode
  | TextLit of string
  | PreLit of string                             (* unresolved numeric literal *)

type unop =
  | PosOp                                        (* +x *)
  | NegOp                                        (* -x *)
  | NotOp                                        (* bitwise negation *)

type binop =
  | AddOp                                        (* x+y *)
  | SubOp                                        (* x-y *)
  | MulOp                                        (* x*y *)
  | DivOp                                        (* x/y *)
  | ModOp                                        (* x%y *)
  | AndOp                                        (* bitwise operators... *)
  | OrOp
  | XorOp
  | ShiftLOp
  | ShiftROp
  | RotLOp
  | RotROp
  | CatOp                                        (* concatenation *)
