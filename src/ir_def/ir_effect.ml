open Ir
open Source
module T = Mo_types.Type

(* a simple effect analysis to annote expressions as Triv(ial) (await-free) or Await (containing unprotected awaits) *)

(* in future we could merge this with the type-checker
   but I prefer to keep it mostly separate for now *)

let max_eff e1 e2 =
  match e1,e2 with
  | T.Triv, T.Triv -> T.Triv
  | _ , T.Await -> T.Await
  | T.Await, _ -> T.Await

let typ phrase = phrase.note.Note.typ
let eff phrase = phrase.note.Note.eff

let is_triv phrase = eff phrase = T.Triv

let effect_exp (exp: exp) : T.eff = eff exp

(* infer the effect of an expression, assuming all sub-expressions are correctly effect-annotated es*)
let rec infer_effect_prim = function
  | ThrowPrim | AwaitPrim -> T.Await
  | _ -> T.Triv

 and infer_effect_exp (exp: exp) : T.eff =
  match exp.it with
  | VarE _
  | LitE _ ->
    T.Triv
  | LabelE (_, _, exp1)
  | LoopE exp1
  | AssignE (_, exp1) ->
    effect_exp exp1
  | PrimE (p, exps) ->
    let es = List.map effect_exp exps in
    List.fold_left max_eff (infer_effect_prim p) es
  | BlockE (ds, exp) ->
    let es = List.map effect_dec ds in
    List.fold_left max_eff (effect_exp exp) es
  | IfE (exp1, exp2, exp3) ->
    let e1 = effect_exp exp1 in
    let e2 = effect_exp exp2 in
    let e3 = effect_exp exp3 in
    max_eff e1 (max_eff e2 e3)
  | SwitchE (exp1, cases) ->
    let e1 = effect_exp exp1 in
    let e2 = effect_cases cases in
    max_eff e1 e2
  | AsyncE _ ->
    T.Triv
  | TryE _ ->
    T.Await
  | DeclareE (_, _, exp1) ->
    effect_exp exp1
  | DefineE (_, _, exp1) ->
    effect_exp exp1
  | FuncE _ ->
    T.Triv
  | SelfCallE (_, _, exp1, exp2) ->
    let e1 = effect_exp exp1 in
    let e2 = effect_exp exp2 in
    max_eff e1 e2
  | ActorE _ ->
    T.Triv
  | NewObjE _ ->
    T.Triv

and effect_cases cases =
  match cases with
  | [] ->
    T.Triv
  | {it = {pat; exp}; _}::cases' ->
    let e = effect_exp exp in
    max_eff e (effect_cases cases')

and effect_dec dec = match dec.it with
  | LetD (_, e) | VarD (_, _, e) -> effect_exp e

let infer_effect_dec = effect_dec

let infer_effect_decs ds =
  let es = List.map effect_dec ds in
  List.fold_left max_eff T.Triv es
