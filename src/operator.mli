open Type
open Value

val unop : typ -> Syntax.unop -> (value -> value)  (* raise Invalid_argument *)
val binop : typ -> Syntax.binop -> (value -> value -> value)  (* raise Invalid_argument *)
val relop : typ -> Syntax.relop -> (value -> value -> value)  (* raise Invalid_argument *)

val has_unop : typ -> Syntax.unop -> bool
val has_binop : typ -> Syntax.binop -> bool
val has_relop : typ -> Syntax.relop -> bool
