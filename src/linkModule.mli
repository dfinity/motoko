(* The arguments are:
   - the base module
   - the name of the library to link
   - the module containing that library
*)
val link : CustomModule.extended_module -> string -> CustomModule.extended_module -> CustomModule.extended_module
