open Source
open Ir
open As_types.Type

val max_eff : eff -> eff -> eff

(* (incremental)  effect inference on IR *)

val typ : ('a, typ_note) annotated_phrase -> typ
val eff : ('a, typ_note) annotated_phrase -> eff

val is_triv : ('a, typ_note) annotated_phrase -> bool

val effect_exp: Ir.exp -> eff
val infer_effect_exp : Ir.exp -> eff
val infer_effect_dec : Ir.dec -> eff
