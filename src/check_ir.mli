type env

val error : env -> Source.region -> ('b, unit, string, 'c) format4 -> 'b

val env_of_scope : Typing.scope -> env

val with_check_exp : (env -> Ir.exp -> unit) -> env -> env
val with_check_typ : (env -> Type.typ -> unit) -> env -> env

val check_prog : env -> Ir.prog -> unit
