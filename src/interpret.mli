module V = Value
module T = Type

type scope = {
  val_env: V.def V.Env.t;
  imp_env: V.value V.Env.t;
}

val empty_scope : scope
val adjoin_scope : scope -> scope -> scope

exception Trap of Source.region * string

val interpret_prog : scope -> Syntax.prog -> V.value option * scope
val interpret_import : scope -> (string * Syntax.prog) -> scope

val print_exn : exn -> unit
