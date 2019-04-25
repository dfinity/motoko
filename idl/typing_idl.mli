module Env : Env.S with type key = string

type val_env = Syntax_idl.typ Env.t

type scope = val_env

val empty_scope : scope

val check_prog : scope -> Syntax_idl.prog -> scope Diag.result

