module Env : Env.S with type key = string

type typ_env = Syntax.typ Env.t

type scope = typ_env

val empty_scope : scope

val check_prog : scope -> Syntax.prog -> scope Diag.result

