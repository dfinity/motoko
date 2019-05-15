exception Trap of Source.region * string

val interpret_prog : Ir.dec list list -> Ir.prog -> unit
