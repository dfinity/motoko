open As_def
open As_types

open Source
open Syntax
open Type

val max_eff : eff -> eff -> eff

(* (incremental) effect inference on Source *)

(* TODO: delete once await.ml ported to IR *)
val effect_exp: exp -> eff
val infer_effect_exp : exp -> eff
val infer_effect_dec : dec -> eff

val typ : ('a, typ_note) annotated_phrase -> typ
val eff : ('a, typ_note) annotated_phrase -> eff

val is_triv : ('a, typ_note) annotated_phrase -> bool
