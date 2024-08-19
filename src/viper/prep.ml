open Common
open Source
open Mo_def.Syntax
open Traversals

module T = Mo_types.Type
module Cons = Mo_types.Cons
module Arrange = Mo_def.Arrange

(*
  Monomorphisation goal. For example, a function call f<Int,Bool>(a,b,c)
  yields a mono_goal of {id="f", typs=[Int,Bool]}
*)
type mono_goal = { mg_id : string; mg_typs : T.typ list; }

let compare_mono_goal (g1 : mono_goal) (g2 : mono_goal) =
  match String.compare g1.mg_id g2.mg_id with
  | 0 -> List.compare T.Ord.compare g1.mg_typs g2.mg_typs
  | ord -> ord

type dec_field_template = { dft_id : string; dft_mk : T.typ list -> dec_field }

let string_of_mono_goal (g : mono_goal) : string =
  String.concat "$" (g.mg_id :: List.map (fun t ->
    match t, T.normalize t with
    | _, T.Prim T.Int  -> "Int"
    | _, T.Prim T.Nat  -> "Nat"
    | _, T.Prim T.Bool -> "Bool"
    | T.Con (con, []), _ -> Mo_types.Cons.name con (* TODO: encode type arguments *)
    | _ -> unsupported Source.no_region (Mo_types.Arrange_type.typ t)) g.mg_typs)

let mono_calls_visitor (stk : mono_goal Stack.t) : visitor =
  { visit_exp = (function
    | {it = CallE({it = VarE v; at = v_at; note = v_note},inst,e); at; note} ->
        let goal = { mg_id = v.it; mg_typs = inst.note } in
        let _ = (if goal.mg_typs = [] then () else Stack.push goal stk) in
        let s = string_of_mono_goal goal in
        {it = CallE({it = VarE (s @~ v_at); at=v_at; note=v_note},
                    {it = None; at=inst.at; note = []}, e); at; note}
    | e -> e);
    visit_typ = Fun.id;
    visit_pat = Fun.id;
    visit_dec = Fun.id;
    visit_inst = Fun.id;
  }

let mono_calls_dec_field (df : dec_field) : (mono_goal list * dec_field) =
  let stk = Stack.create () in
  let df' = over_dec_field (mono_calls_visitor stk) df in
  let goals = Stack.fold (fun gs g -> g :: gs) [] stk in
  (goals, df')

let mono_calls_dec_fields (dfs : dec_field list) : (mono_goal list * dec_field list) =
  let stk = Stack.create () in
  let dfs' = List.map (over_dec_field (mono_calls_visitor stk)) dfs in
  let goals = Stack.fold (fun gs g -> g :: gs) [] stk in
  (goals, dfs')

module M = Map.Make(String)
module MonoGoalEnv = Map.Make(struct type t = mono_goal let compare = compare_mono_goal end)

type subst_env = T.typ T.ConEnv.t   (* con -> typ *)

let unwrap_typ_bind_con (tb : typ_bind) : T.con =
  match tb.note with
  | None -> unsupported tb.at (Arrange.typ_bind tb)
  | Some(c) -> c

let init_subst_env (tbs : typ_bind list) (ts : T.typ list) : subst_env =
  T.ConEnv.from_list2 (List.map unwrap_typ_bind_con tbs) ts

let subst_visitor (env : subst_env) : visitor =
  { visit_exp = (fun e -> { e with note = { e.note with note_typ = T.subst env e.note.note_typ } });
    visit_typ = (fun t -> { t with note = T.subst env t.note });
    visit_pat = (fun p -> { p with note = T.subst env p.note });
    visit_dec = (fun d -> { d with note = { d.note with note_typ = T.subst env d.note.note_typ } });
    visit_inst = (fun i -> { i with note = List.map (T.subst env) i.note; }) }

let subst_exp (env : subst_env) : exp -> exp = over_exp (subst_visitor env)
let subst_pat (env : subst_env) : pat -> pat = over_pat (subst_visitor env)
let subst_typ (env : subst_env) : typ -> typ = over_typ (subst_visitor env)

let mk_template_dec_field (df : dec_field) : dec_field_template option =
  match (df.it.vis.it, df.it.dec.it) with
    | (Private, LetD({it = VarP(_);at=p_at;note=p_note},
                     {it = FuncE(x,sp,tp,p,t,sugar,e);
                      at=fn_at;
                      note=fn_note},
                 None)) ->
      if tp = [] then None else
      Some({dft_id = x;
            dft_mk = fun typs ->
              let env = init_subst_env tp typs in
              (* let _ = T.ConEnv.iter (fun c t -> print_endline (String.concat " " [Cons.to_string true ":" c; "="; T.string_of_typ t])) env in *)
              let p' = subst_pat env p in
              let t' = Option.map (subst_typ env) t in
              let e' = subst_exp env e in
              let x' = string_of_mono_goal ({ mg_id = x; mg_typs = typs }) in
              { df with it = { df.it with dec = { df.it.dec with it =
                LetD({it = VarP({it=x';at=Source.no_region;note=()});
                      at=p_at;
                      note=p_note},
                     {it = FuncE(x',sp,[],p',t',sugar,e');
                      at=fn_at;
                      note=fn_note},
                  None) } } }
      })
    | _ -> None

let mono_partition (dfs : dec_field list) : (dec_field list * dec_field_template M.t) =
  let (dfs', tmpls) =
    List.partition_map
      (fun df ->
        match mk_template_dec_field df with
        | None -> Either.Left df
        | Some(dft) -> Either.Right dft)
      dfs
  in
  let tmpls' = List.fold_left (fun acc tmpl -> M.add tmpl.dft_id tmpl acc) M.empty tmpls in
  (dfs', tmpls')

let rec mono_iterate (ts : dec_field_template M.t) (dfs : dec_field MonoGoalEnv.t) (goals : mono_goal list) : dec_field MonoGoalEnv.t =
  match goals with
  | [] -> dfs
  | g :: gs ->
      if MonoGoalEnv.mem g dfs then mono_iterate ts dfs gs else (* skip already instantiated goals *)
      let df = (M.find g.mg_id ts).dft_mk g.mg_typs in  (* instantiate the goal *)
      let (gs', df') = mono_calls_dec_field df in       (* collect new goals *)
      mono_iterate ts (MonoGoalEnv.add g df' dfs) (List.append gs' gs)

let mono_dec_fields (dfs : dec_field list) : dec_field list =
  let (base_dfs, tmpls) = mono_partition dfs in
  let (base_goals, base_dfs') = mono_calls_dec_fields base_dfs in
  let mono_dfs = mono_iterate tmpls MonoGoalEnv.empty base_goals in
  MonoGoalEnv.fold (fun _ df dfs -> df :: dfs) mono_dfs base_dfs'

let prep_unit (u : comp_unit) : comp_unit =
  let { imports; body } = u.it in
  match body.it with
  | ActorU(id_opt, decs) ->
    let decs' = mono_dec_fields decs in
    (* let _ = List.map (fun g -> print_endline (string_of_mono_goal g)) goals in *)
    let body' = ActorU(id_opt, decs') in
    (* let _ = List.map (fun d -> print_endline (Wasm.Sexpr.to_string 80 (Arrange.dec_field d))) decs' in *)
    { u with it = {imports; body = { body with it = body' } } }
  | _ -> u
