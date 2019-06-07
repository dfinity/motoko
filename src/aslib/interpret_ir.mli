module V = Value
module T = Type

type flags = {
  trace : bool;
  print_depth : int
}

type scope = V.def V.Env.t

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

exception Trap of Source.region * string

val interpret_prog : flags -> scope -> Ir.prog -> scope
