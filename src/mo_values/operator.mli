open Mo_types.Type
open Value

(* Operators *)

type unop =
  | PosOp                                       (* +x *)
  | NegOp                                       (* -x *)
  | NotOp                                       (* bitwise negation *)

type binop =
  | AddOp                                       (* x+y *)
  | SubOp                                       (* x-y *)
  | MulOp                                       (* x*y *)
  | DivOp                                       (* x/y *)
  | ModOp                                       (* x%y *)
  | PowOp                                       (* x^y *)
  | AndOp                                       (* bitwise operators... *)
  | OrOp
  | XorOp
  | ShLOp
  | ShROp
  | RotLOp
  | RotROp
  | WAddOp                                      (* wrapping operators... *)
  | WSubOp
  | WMulOp
  | WPowOp
  | CatOp                                       (* concatenation *)

type relop =
  | EqOp                                        (* x=y *)
  | NeqOp                                       (* x!=y *)
  | LtOp                                        (* x<y *)
  | GtOp                                        (* x>y *)
  | LeOp                                        (* x<=y *)
  | GeOp                                        (* x>=y *)


val unop : unop -> typ -> (value -> value)  (* raise Invalid_argument *)
val binop : binop -> typ -> (value -> value -> value)  (* raise Invalid_argument *)
val relop : relop -> typ -> (value -> value -> value)  (* raise Invalid_argument *)

val has_unop : unop -> typ -> bool
val has_binop : binop -> typ -> bool
val has_relop : relop -> typ -> bool

val type_unop : unop -> typ -> typ
val type_binop : binop -> typ -> typ
val type_relop : relop -> typ -> typ
