open Mo_types
val prim : string -> Value.func
val num_conv_trap_prim : Type.prim -> Type.prim -> Value.value -> Value.value
val num_conv_wrap_prim : Type.prim -> Type.prim -> Value.value -> Value.value
