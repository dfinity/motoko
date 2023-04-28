(* Turns rebuilt variants into casts of the same thing. *)

open Ir_def
open Ir
open Source

let empty_env () = ()

(* The AST traversal *)

(* Does a very simple thing (for now):
 - spot `switch var {`
 - where a variant match happens, that immediately recombines:
    `case (#err err) { #err err }`
 - replace with `case (#err _) { (prim cast) var }`
 *)

let rec t_exps env = List.map (t_exp env)

and t_exp env (e : Ir.exp) =
  { e with it = t_exp' env e.it }

and t_exp' env = function
  | LitE l -> LitE l
  | VarE id -> VarE id
  | PrimE (p, es) -> PrimE (p, t_exps env es)
  | AssignE (lexp1, exp2) ->
    AssignE (t_lexp env lexp1, t_exp env exp2)
  | FuncE (s, c, id, typbinds, pat, typT, exp) ->
    FuncE (s, c, id, typbinds, pat, typT, t_exp env exp)
  | BlockE block -> BlockE (t_block env block)
  | IfE (exp1, exp2, exp3) ->
    IfE (t_exp env exp1, t_exp env exp2, t_exp env exp3)
  | SwitchE (exp1, cases) ->
    let exp1' = t_exp env exp1 in
    let cases' =
      List.map
        (function
         | {it = { pat = { it = TagP (ptag, ({it = VarP pid; _} as pv)); _ } as pat; exp = { it = PrimE (TagPrim etag, [{ it = VarE _; _}]); _ } as exp }; _} as case ->
            { case with it = {pat = { pat with it = TagP (ptag, {pv with it = WildP})}; exp = { exp with it = PrimE (CastPrim (exp1.note.Note.typ, exp.note.Note.typ), [exp1]) } } }
         | {it = { pat; exp }; _} as case ->
            { case with it = {pat; exp = t_exp env exp} })
        cases
    in
    SwitchE (exp1', cases')
  | TryE (exp1, cases) ->
    let cases' =
      List.map
        (fun ({it = { pat; exp }; _} as case) ->
          {case with it = {pat; exp = t_exp env exp} })
        cases
    in
    TryE (t_exp env exp1, cases')
  | LoopE exp1 ->
    LoopE (t_exp env exp1)
  | LabelE (id, typ, exp1) ->
    LabelE (id, typ, t_exp env exp1)
  | AsyncE (s, tb, e, typ) -> AsyncE (s, tb, t_exp env e, typ)
  | DeclareE (id, typ, exp1) ->
    DeclareE (id, typ, t_exp env exp1)
  | DefineE (id, mut ,exp1) ->
    DefineE (id, mut, t_exp env exp1)
  | NewObjE (sort, ids, t) ->
    NewObjE (sort, ids, t)
  | SelfCallE (ts, e1, e2, e3) ->
    SelfCallE (ts, t_exp env e1, t_exp env e2, t_exp env e3)
  | ActorE (ds, fields, {meta; preupgrade; postupgrade; heartbeat; timer; inspect}, typ) ->
    (* Until Actor expressions become their own units,
       we repeat what we do in `comp_unit` below *)
    let env1 = empty_env () in
    let ds' = t_decs env1 ds in
    let preupgrade' = t_exp env1 preupgrade in
    let postupgrade' = t_exp env1 postupgrade in
    let heartbeat' = t_exp env1 heartbeat in
    let timer' = t_exp env1 timer in
    let inspect' = t_exp env1 inspect in
    ActorE (ds', fields,
      {meta;
       preupgrade = preupgrade';
       postupgrade = postupgrade';
       heartbeat = heartbeat';
       timer = timer';
       inspect = inspect'
      }, typ)

and t_lexp env (e : Ir.lexp) = { e with it = t_lexp' env e.it }
and t_lexp' env = function
  | VarLE id -> VarLE id
  | IdxLE (exp1, exp2) ->
    IdxLE (t_exp env exp1, t_exp env exp2)
  | DotLE (exp1, n) ->
    DotLE (t_exp env exp1, n)

and t_dec env dec = { dec with it = t_dec' env dec.it }

and t_dec' env dec' =
  match dec' with
  | LetD (pat,exp) -> LetD (pat,t_exp env exp)
  | VarD (id, typ, exp) -> VarD (id, typ, t_exp env exp)
  | RefD (id, typ, lexp) -> RefD (id, typ, t_lexp env lexp)

and t_decs env decs = List.map (t_dec env) decs

and t_block env (ds, exp) = (t_decs env ds, t_exp env exp)

and t_comp_unit = function
  | LibU _ -> raise (Invalid_argument "cannot compile library")
  | ProgU ds ->
    let env = empty_env () in
    let ds' = t_decs env ds in
    ProgU  ds'
  | ActorU (as_opt, ds, fields, {meta; preupgrade; postupgrade; heartbeat; timer; inspect}, typ) ->
    let env = empty_env () in
    let ds' = t_decs env ds in
    let preupgrade' = t_exp env preupgrade in
    let postupgrade' = t_exp env postupgrade in
    let heartbeat' = t_exp env heartbeat in
    let timer' = t_exp env timer in
    let inspect' = t_exp env inspect in
    ActorU (as_opt, ds', fields,
      {meta;
       preupgrade = preupgrade';
       postupgrade = postupgrade';
       heartbeat = heartbeat';
       timer = timer';
       inspect = inspect'
      }, typ)

(* Entry point for the program transformation *)

let transform (cu, flavor) =
  assert (not flavor.has_typ_field); (* required for hash_typ *)
  (t_comp_unit cu, {flavor with has_poly_eq = false})
