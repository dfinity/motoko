open Wasm.Sexpr
open Operator

let unop uo = match uo with
  | PosOp -> Atom "PosOp"
  | NegOp -> Atom "NegOp"
  | NotOp -> Atom "NotOp"

let binop bo = match bo with
  | AddOp  -> Atom "AddOp"
  | SubOp  -> Atom "SubOp"
  | MulOp  -> Atom "MulOp"
  | DivOp  -> Atom "DivOp"
  | ModOp  -> Atom "ModOp"
  | AndOp  -> Atom "AndOp"
  | OrOp   -> Atom "OrOp"
  | XorOp  -> Atom "XorOp"
  | ShLOp  -> Atom "ShiftLOp"
  | ShROp  -> Atom "ShiftROp"
  | RotLOp -> Atom "RotLOp"
  | RotROp -> Atom "RotROp"
  | CatOp  -> Atom "CatOp"
  | PowOp  -> Atom "PowOp"
  | WrappingAddOp  -> Atom "WrappingAddOp"
  | WrappingSubOp  -> Atom "WrappingSubOp"
  | WrappingMulOp  -> Atom "WrappingMulOp"
  | WrappingDivOp  -> Atom "WrappingDivOp"
  | WrappingPowOp  -> Atom "WrappingPowOp"

let relop ro = match ro with
  | EqOp  -> Atom "EqOp"
  | NeqOp -> Atom "NeqOp"
  | LtOp  -> Atom "LtOp"
  | GtOp  -> Atom "GtOp"
  | LeOp  -> Atom "LeOp"
  | GeOp  -> Atom "GeOp"
