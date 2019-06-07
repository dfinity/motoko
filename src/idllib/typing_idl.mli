module Env : Env_idl.S with type key = string

type typ_env = Syntax_idl.typ Env.t

type scope = typ_env

val empty_scope : scope

val check_prog : scope -> Syntax_idl.prog -> scope Diag.result

