open Syntax
open Source
module T = Type

(* a simple effect analysis to annote expressions as Triv(ial) (await-free) or Await (containing unprotected awaits) *)

(* currently not used, but will be for selective cps conversion *)

let max_eff e1 e2 =
  match e1,e2 with
  | T.Triv,T.Triv -> T.Triv
  | _ , T.Await -> T.Await
  | T.Await,_ -> T.Await

let rec infer_exp (exp:Syntax.exp) : T.eff =
  let e = infer_exp exp in
  exp.note <- (e,snd(exp.note));
  e

and infer_exp' (exp:Syntax.exp) : T.eff =
  match exp.it with
  | VarE _ 
  | LitE _ ->
    T.Triv
  | UnE (_, exp1) 
  | ProjE (exp1, _) 
  | DotE (exp1, _) 
  | NotE exp1
  | AssertE exp1 
  | LabelE (_, exp1) 
  | BreakE (_, exp1) 
  | RetE exp1   
  | IsE (exp1, _) 
  | AnnotE (exp1, _) 
  | LoopE (exp1, None) -> 
    infer_exp exp1
  | BinE (exp1, _, exp2)
  | IdxE (exp1, exp2) 
  | RelE (exp1, _, exp2) 
  | AssignE (exp1, exp2) 
  | CallE (exp1, _, exp2) 
  | AndE (exp1, exp2)
  | OrE (exp1, exp2) 
  | WhileE (exp1, exp2) 
  | LoopE (exp1, Some exp2) 
  | ForE (_, exp1, exp2)-> 
    let t1 = infer_exp exp1 in
    let t2 = infer_exp exp2 in
    max_eff t1 t2
  | TupE exps 
  | ArrayE exps 
  | BlockE exps ->
    let es = List.map infer_exp exps in
    List.fold_left max_eff Type.Triv es 
  | ObjE (_, _, efs) ->
    infer_field_exps efs 
  | IfE (exp1, exp2, exp3) ->
    let e1 = infer_exp exp1 in
    let e2 = infer_exp exp2 in
    let e3 = infer_exp exp3 in
    max_eff e1 (max_eff e2 e3)
  | SwitchE (exp1, cases) ->
    let e1 = infer_exp exp1 in
    let e2 = infer_cases cases in
    max_eff e1 e2
  | AsyncE exp1 ->
    let _ = infer_exp exp1 in
    T.Triv
  | AwaitE exp1 ->
    let _ = infer_exp exp1 in
    T.Await 
  | DecE d ->
    infer_dec d
    
and infer_cases cases =
  match cases with
  | [] ->
    T.Triv
  | {it = {pat; exp}; _}::cases' ->
    let e = infer_exp exp in
    max_eff e (infer_cases cases')

and infer_block es =
  List.fold_left max_eff T.Triv es

and infer_field_exps efs =
  List.fold_left (fun e (fld:exp_field) -> max_eff e (infer_exp fld.it.exp)) T.Triv efs

and infer_dec d =
  match d.it with
  | LetD(_,e) 
  | VarD (_, e) ->
    infer_exp e
  | TypD (v, tps, t) ->
    T.Triv
  | FuncD (v, tps, p, t, e) ->
    let _ = infer_exp e in
    T.Triv
  | ClassD (a, v, tps, p, efs) ->
    infer_field_exps efs 
