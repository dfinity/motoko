open As_ir
open As_types
open As_values

val can_show : Type.typ -> bool

val show_val : Type.typ -> Value.value -> string

val transform : 'a -> Ir.prog -> Ir.prog
