module V = Value
module T = Type

type scope = {
  val_env: V.def V.Env.t;
  lib_env: V.value V.Env.t;
}

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

exception Trap of Source.region * string

val interpret_prog : bool -> int -> scope -> Syntax.prog -> (V.value * scope) option
val interpret_library : bool -> int -> scope -> Syntax.library -> scope
