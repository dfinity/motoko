module V = Value
module T = Type

type scope = V.def V.Env.t

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

exception Trap of Source.region * string

val interpret_prog : scope -> Ir.prog -> scope
