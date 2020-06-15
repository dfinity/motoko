open Mo_def
open Ir_def

(* These declaratoins are all internal, either from prelude (@-prefixed)
   or to bring libraries into scope *)
type import_declaration = Ir.dec list

val import_lib : Syntax.lib -> import_declaration
val import_class : string -> string -> import_declaration
val import_prelude : Syntax.prog -> import_declaration

val transform_unit : Syntax.comp_unit -> Ir.prog

val link_declarations : import_declaration -> Ir.prog -> Ir.prog
