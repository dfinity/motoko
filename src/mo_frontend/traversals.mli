open Mo_def.Syntax

val over_exp: (exp -> exp) -> exp -> exp
val over_dec: (exp -> exp) -> dec -> dec
val over_dec_field: (exp -> exp) -> dec_field -> dec_field
val over_case: (exp -> exp) -> case -> case
val over_prog: (exp -> exp) -> prog -> prog
val on_import_over_prog: (pat -> exp -> string * resolved_import ref -> unit) -> prog -> unit
