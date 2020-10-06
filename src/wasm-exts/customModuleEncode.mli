val encode : CustomModule.extended_module -> string * string

val promise_reference_slot : int Lib.Promise.t -> int
val allocate_reference_slot : unit -> int
