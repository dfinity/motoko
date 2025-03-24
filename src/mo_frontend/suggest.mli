open Mo_types

val suggest_id : string (* desc *) -> string (* id *) -> string list (* ids *) -> string

val suggest_conversion : Scope.lib_env -> (Type.typ * _ * _ * _) Type.Env.t -> Type.typ -> Type.typ -> string
