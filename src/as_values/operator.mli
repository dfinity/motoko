open As_types.Type
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
  | UShROp
  | SShROp
  | RotLOp
  | RotROp
  | CatOp                                       (* concatenation *)

type relop =
  | EqOp                                        (* x=y *)
  | NeqOp                                       (* x!=y *)
  | LtOp                                        (* x<y *)
  | GtOp                                        (* x>y *)
  | LeOp                                        (* x<=y *)
  | GeOp                                        (* x>=y *)


val unop : typ -> unop -> (value -> value)  (* raise Invalid_argument *)
val binop : typ -> binop -> (value -> value -> value)  (* raise Invalid_argument *)
val relop : typ -> relop -> (value -> value -> value)  (* raise Invalid_argument *)

val has_unop : typ -> unop -> bool
val has_binop : typ -> binop -> bool
val has_relop : typ -> relop -> bool
