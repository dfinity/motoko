(* The first argument specifies the global of the first module indicating the
start of free memory *)
(* The third argument the name of the moduled linked in *)
val link : int32 -> CustomModule.extended_module -> string -> Dylib.dylink_module -> CustomModule.extended_module
