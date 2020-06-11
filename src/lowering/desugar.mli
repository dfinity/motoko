open Mo_def
open Ir_def

(* These declaratoins are all internal, either from prelude (@-prefixed)
   or to bring libraries into scope *)
type import_declaration = Ir.dec list

val transform_lib : Syntax.lib -> import_declaration
val transform_prelude : Syntax.prog -> import_declaration
val transform_prog : Syntax.prog -> Ir.prog
val link_declarations : import_declaration -> Ir.prog -> Ir.prog
