exception Code of Wasm.Source.region * string

val version : int32
(*val encode : Ast.module_ -> string
val encode_custom : Ast.name -> string -> string*)

val encode : CustomModule.extended_module -> string

val promise_reference_slot : int Lib.Promise.t -> int
val allocate_reference_slot : unit -> int
