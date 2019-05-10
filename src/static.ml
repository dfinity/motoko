(*
This module implements the staticity check, needed for modules and imported
files.

The guiding principle is: Static expressions are expressions that can be
compiled to values without evaluation.

There is some mushiness around let-expressions and variables, which do form
some kind of beta-reduction, and can actually cause loops, but are required to
allow re-exporting names in modules.
*)

open Source
open Syntax

let err m at =
  let open Diag in
  add_msg m {
    sev = Diag.Error;
    cat = "type";
    at;
    text = "non-static expression in library or module"
  }

let pat_err m at =
  let open Diag in
  add_msg m {
    sev = Diag.Error;
    cat = "type";
    at;
    text = "only trivial patterns allowed in static expressions"
  }

let rec exp m e = match e.it with
  (* Plain values *)
  | (PrimE _ | LitE _ | FuncE _) -> ()
  | (VariantE (_, exp1) | OptE exp1) -> exp m exp1
  | (TupE es | ArrayE (_, es)) -> List.iter (exp m) es
  | ObjE (_, efs) -> List.iter (fun ef -> dec m ef.it.dec) efs

  (* Variable access. Dangerous, due to loops. *)
  | (VarE _ | ImportE _) -> ()

  (* Projections. These are a form of evaluation. *)
  | ProjE (exp1, _)
  | DotE (exp1, _) -> exp m exp1
  | IdxE (exp1, exp2) -> exp m exp1; exp m exp2

  (* Transparent *)
  | AnnotE (exp1, _) -> exp m exp1
  | BlockE ds -> List.iter (dec m) ds

  (* Clearly non-static *)
  | UnE _
  | ShowE _
  | NotE _
  | AssertE _
  | LabelE _
  | BreakE _
  | RetE _
  | AsyncE _
  | AwaitE _
  | LoopE _
  | BinE _
  | RelE _
  | AssignE _
  | CallE _
  | AndE _
  | OrE _
  | WhileE _
  | ForE _
  | IfE _
  | SwitchE _
  -> err m e.at

and dec m d = match d.it with
  | TypD _ | ClassD _ -> ()
  | ExpD e -> exp m e
  | LetD (p, e) -> triv m p; exp m e
  | VarD _ -> err m d.at
(*  | ModuleD (_, ds) -> List.iter (dec m) ds *)

and triv m p = match p.it with
  | (WildP | VarP _) -> ()

  (*
  If we allow projections above, then we should allow irrefutable
  patterns here.
  *)
  | TupP ps -> List.iter (triv m) ps

  (* Everything else is forbidden *)
  | _ -> pat_err m p.at

let prog p =
  Diag.with_message_store (fun m -> List.iter (dec m) p.it; Some ())
