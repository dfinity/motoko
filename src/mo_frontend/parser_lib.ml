open Mo_def

open Syntax
open Source

exception Imports of Mo_def.Syntax.dec list

(* Temporary hack! *)
let msg_store : Diag.msg_store option ref = ref None
let mode : Lexer_lib.mode option ref = ref None

let triv_table : Trivia.triv_table ref = ref Trivia.empty_triv_table

(* Loop parsing helpers *)
let auto_s = "<>auto"
let auto_continue_s = "continue <>auto"

let label_s s (e : exp) : exp =
  LabelE(s @@ e.at, TupT [] @! e.at, e) @? e.at

let replace_label x (e : exp) : exp =
  match e.it with
  | LabelE (_, {it = rt; _}, e) -> LabelE (x, rt @! x.at, e) @? e.at
  | _ -> assert false

let auto_loop_labels (e : exp) : exp =
  let e' =
    match e.it with
    | WhileE (e1, e2) -> WhileE (e1, label_s auto_continue_s e2) @? e.at
    (* | LoopE (e1, eo) -> LoopE (label_s auto_continue_s e1, eo) @? e.at *)
    | ForE (p, e1, e2) -> ForE (p, e1, label_s auto_continue_s e2) @? e.at
    | _ -> e
  in
  label_s auto_s e'

let replace_auto_loop_labels at x (rt : typ option) e : exp =
  let x' = ("continue " ^ x.it) @@ x.at in
  let rt = Option.value rt ~default:(TupT [] @! at) in
  match e.it with
  | LabelE (a, _, e) when a.it = auto_s ->
    let e' =
      match e.it with
      | WhileE (e1, e2) -> WhileE (e1, replace_label x' e2) @? e.at
      (* | LoopE (e1, eo) -> LoopE (replace_label x' e1, eo) @? e.at *)
      | ForE (p, e1, e2) -> ForE (p, e1, replace_label x' e2) @? e.at
      | _ -> assert false
    in
    LabelE(x, rt, e') @? at
  | _ -> 
    (* Not a loop label, so no replacement needed *)
    e
