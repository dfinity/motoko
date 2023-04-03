open Mo_types
type trap = { trap : 'a. string -> 'a  }
val prim : trap -> string -> Value.func
val num_conv_trap_prim : trap -> Type.prim -> Type.prim -> Value.value -> Value.value
val num_conv_wrap_prim : trap -> Type.prim -> Type.prim -> Value.value -> Value.value
