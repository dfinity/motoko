val is_show_func_ir : Ir.exp -> bool
val is_show_func : Syntax.exp -> bool

val can_show : Type.typ -> bool

val show_val : Type.typ -> Value.value -> string

val transform : 'a -> Ir.prog -> Ir.prog
