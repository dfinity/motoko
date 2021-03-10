(* Translates away calls to the `TypRep` prim *)

(* NB: This code re-caluculates the typ_hash many times. Could be optimized. *)

open Ir_def
open Mo_types

open Source
open Ir
module T = Type
open Construct
open Typ_hash

(* Environment *)

(* We go through the file and collect all type arguments to `show`.
   We store them in `params`, indexed by their `type_id`
*)

module M = Map.Make(String)
type env =
  { params : T.typ M.t ref
  }

let empty_env () : env = {
  params = ref M.empty;
  }

let add_type env t : unit =
  env.params := M.add (typ_hash t) t !(env.params)

(* Definition names *)

let name_for t = "@typ_rep<" ^ typ_hash t ^ ">"
let var_for t : Construct.var = var (name_for t) T.(Array (Mut typRepT))
let exp_for t : exp = tagE "ref" (varE (var_for t))
let lexp_for t : lexp =
  { it = IdxLE (varE (var_for t), natE Mo_values.Numerics.Int.zero)
  ; at = no_region
  ; note = T.(Mut typRepT)
  }

(* Synthesizing a single show function *)

(* Returns the new declarations, as well as a list of further types it needs *)

let rhs_for : T.typ -> Ir.exp = fun t ->
  match T.normalize t with
  | T.(Prim Bool) ->
    tagE "bool" unitE
  | T.(Prim Int) ->
    tagE "int" unitE
  | T.Tup ts ->
    tagE "tup" (arrayE typRepT (List.map exp_for ts))
  | T.Opt t ->
    tagE "opt" (exp_for t)
  | _ ->
    raise (Invalid_argument ("Typrep.decl_for :" ^ T.string_of_typ_expand t))

let subterms_of : T.typ -> T.typ list = fun t ->
  match T.normalize t with
  | T.Tup ts -> ts
  | T.Opt t -> [t]
  | _ -> []

(* Synthesizing the types recursively. Hopefully well-founded. *)

let decls : T.typ M.t -> Ir.dec list = fun roots ->
  let roots = List.map snd (M.bindings roots) in

  (* Enumerate all subterms (Depth-first traversal) *)
  let seen = ref M.empty in
  let rec go = function
    | [] -> ()
    | t::todo when M.mem (typ_hash t) !seen ->
      go todo
    | t::todo ->
      seen := M.add (typ_hash t) t !seen;
      go (subterms_of t @ todo)
  in go roots;

  (* Now generate declarations *)
  List.map (fun (_h, t) ->
    letD (var_for t) (mutArrayE typRepT [tagE "any" unitE])
  ) (M.bindings !seen) @
  List.map (fun (_h, t) ->
    expD {
      it = AssignE (lexp_for t, rhs_for t);
      at = no_region;
      note = Note.{def with typ = T.unit}
    }
  ) (M.bindings !seen)


(* The AST traversal *)

(* Does two things:
 - collects all uses of `debug_show` in the `env`
 - for each actor, resets the environment, recurses,
   and adds the show functions (this keeps closed actors closed)
*)

let rec t_exps env decs = List.map (t_exp env) decs

and t_exp env (e : Ir.exp) =
  { e with it = t_exp' env e.it }

and t_exp' env = function
  | LitE l -> LitE l
  | VarE id -> VarE id
  | PrimE (TypRep ot, []) ->
    let t' = T.normalize ot in
    add_type env t';
    (exp_for t').it
  | PrimE (p, es) -> PrimE (p, t_exps env es)
  | AssignE (lexp1, exp2) ->
    AssignE (t_lexp env lexp1, t_exp env exp2)
  | FuncE (s, c, id, typbinds, pat, typT, exp) ->
    FuncE (s, c, id, typbinds, pat, typT, t_exp env exp)
  | BlockE block -> BlockE (t_block env block)
  | IfE (exp1, exp2, exp3) ->
    IfE (t_exp env exp1, t_exp env exp2, t_exp env exp3)
  | SwitchE (exp1, cases) ->
    let cases' =
      List.map
        (fun {it = {pat;exp}; at; note} ->
          {it = {pat = pat; exp = t_exp env exp}; at; note})
        cases
    in
    SwitchE (t_exp env exp1, cases')
  | TryE (exp1, cases) ->
    let cases' =
      List.map
        (fun {it = {pat;exp}; at; note} ->
          {it = {pat = pat; exp = t_exp env exp}; at; note})
        cases
    in
    TryE (t_exp env exp1, cases')
  | LoopE exp1 ->
    LoopE (t_exp env exp1)
  | LabelE (id, typ, exp1) ->
    LabelE (id, typ, t_exp env exp1)
  | AsyncE (tb, e, typ) -> AsyncE (tb, t_exp env e, typ)
  | DeclareE (id, typ, exp1) ->
    DeclareE (id, typ, t_exp env exp1)
  | DefineE (id, mut ,exp1) ->
    DefineE (id, mut, t_exp env exp1)
  | NewObjE (sort, ids, t) ->
    NewObjE (sort, ids, t)
  | SelfCallE (ts, e1, e2, e3) ->
    SelfCallE (ts, t_exp env e1, t_exp env e2, t_exp env e3)
  | ActorE (ds, fields, {pre; post}, typ) ->
    (* Until Actor expressions become their own units,
       we repeat what we do in `comp_unit` below *)
    let env1 = empty_env () in
    let ds' = t_decs env1 ds in
    let pre' = t_exp env1 pre in
    let post' = t_exp env1 post in
    let ds = decls !(env1.params) in
    ActorE (ds @ ds', fields, {pre = pre'; post = post'}, typ)

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

and t_decs env decs = List.map (t_dec env) decs

and t_block env (ds, exp) = (t_decs env ds, t_exp env exp)

and t_comp_unit = function
  | LibU _ -> raise (Invalid_argument "cannot compile library")
  | ProgU ds ->
    let env = empty_env () in
    let ds' = t_decs env ds in
    let ds = decls !(env.params) in
    ProgU (ds @ ds')
  | ActorU (as_opt, ds, fields, {pre; post}, typ) ->
    let env = empty_env () in
    let ds' = t_decs env ds in
    let pre' = t_exp env pre in
    let post' = t_exp env post in
    let ds = decls !(env.params) in
    ActorU (as_opt, ds @ ds', fields, {pre = pre'; post = post'}, typ)

(* Entry point for the program transformation *)

let transform (cu, flavor) =
  (t_comp_unit cu, flavor)
