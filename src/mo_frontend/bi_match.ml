open Mo_types
open Type

(* TODO: consider turning off show_stamps (but then do it elsewhere too)
open MakePretty(struct let show_stamps = false end)
*)

(* Turn on/off debug prints *)
let debug = false

let pp_rel ppf (t1, rel, t2) =
  Format.fprintf ppf "@[<hv 2>%a  %s @ %a@]"
    pp_typ t1
    rel
    pp_typ t2

let pp_constraint ppf (lb, c, ub) =
  Format.fprintf ppf "@[<hv 2>%a  <: @ @[<hv 2>%s  <: @ %a@]@]"
    pp_typ lb
    (Cons.name c)
    pp_typ ub

let display_constraint = Lib.Format.display pp_constraint
let display_rel = Lib.Format.display pp_rel

(* Bi-Matching *)

exception Bimatch of string

type var_info = {
  (* Type.Con for this type variable *)
  t : typ;
  (* Input type parameter; used for validation *)
  bind : bind;
}

type ctx = {
  (* Set of type variables being solved *)
  var_set : ConSet.t;
  (* Type variables info *)
  var_env : var_info ConEnv.t;
  (* List that preserves the order of input type parameters *)
  var_list : con list;
  (* Lower and upper bounds for type variables *)
  bounds : typ ConEnv.t * typ ConEnv.t;
  (* Variances for type variables *)
  variances : Variance.t ConEnv.t;
  (* Optional subtyping constraints to verify the solution in the last round *)
  to_verify : typ list * typ list;
}

let empty_ctx = {
  var_set = ConSet.empty;
  var_env = ConEnv.empty;
  var_list = [];
  bounds = (ConEnv.empty, ConEnv.empty);
  variances = ConEnv.empty;
  to_verify = ([], []);
}

let is_ctx_empty ctx = ConSet.is_empty ctx.var_set

let verify_ctx ctx =
  assert (ConSet.equal ctx.var_set (ConEnv.dom ctx.var_env));
  assert (ConSet.equal ctx.var_set (ConEnv.dom ctx.variances));
  assert (ConSet.equal ctx.var_set (ConEnv.dom (fst ctx.bounds)));
  assert (ConSet.equal ctx.var_set (ConEnv.dom (snd ctx.bounds)));
  assert (ConSet.equal ctx.var_set (ConSet.of_list ctx.var_list))

let string_of_bounds (l, u) =
  String.concat ", " (List.map (fun (c, t) -> Printf.sprintf "%s <: %s <: %s" (string_of_typ t) (Cons.name c) (string_of_typ (ConEnv.find c u))) (ConEnv.bindings l))

module SS = Set.Make (OrdPair)

(* Types that are denotable (ranged over) by type variables *)
let denotable t =
  let t' = normalize t in
  not (is_mut t' || is_typ t')

let bound c = match Cons.kind c with
  | Abs ([], t) -> t
  | _ -> assert false

let as_con_var t = match as_con t with
  | c, [] -> c
  | _ -> assert false

let is_unsolved_var ctx t =
  match t with
  | Con (c, []) -> ConSet.mem c ctx.var_set
  | _ -> false

(* Check partial instantiation `env` satisfies bounds and all the pairwise sub-typing relations in `(ts1, ts2)`;
   used to sanity check inferred instantiations *)
let verify_inst ~ctx ~remaining env (ts1, ts2) =
  List.length (ConEnv.keys ctx.var_env) = List.length (ConEnv.keys env) &&
  ConEnv.for_all (fun c { t; bind } ->
    (* NB: bounds are closed, no need to substitute *)
    is_unsolved_var remaining t || sub (ConEnv.find c env) bind.bound) ctx.var_env &&
  List.for_all2 (fun t1 t2 -> sub (subst env t1) (subst env t2)) ts1 ts2

let mentions typ cons = not (ConSet.disjoint (Type.cons typ) cons)

let fail_open_bound c bd =
  let c = Cons.name c in
  raise (Bimatch (Format.asprintf
    "type parameter %s has an open bound%a\nmentioning another type parameter, so that explicit type instantiation is required due to limitation of inference"
    c (Lib.Format.display pp_typ) bd))

let choose_under_constrained ctx lb c ub =
  match ConEnv.find c ctx.variances with
  | Variance.Covariant -> lb
  | Variance.Contravariant -> ub
  | Variance.Bivariant -> lb
  | Variance.Invariant ->
    raise (Bimatch (Format.asprintf
      "implicit instantiation of type parameter %s is under-constrained with%a\nwhere%a\nso that explicit type instantiation is required"
      (Cons.name c)
      display_constraint (lb, c, ub)
      display_rel (lb,"=/=",ub)))

let fail_over_constrained lb c ub =
  raise (Bimatch (Format.asprintf
    "implicit instantiation of type parameter %s is over-constrained with%a\nwhere%a\nso that no valid instantiation exists"
    (Cons.name c)
    display_constraint (lb, c, ub)
    display_rel (lb, "</:", ub)))

let bi_match_typs ctx =
  let flexible c = ConSet.mem c ctx.var_set in
  
  let rec bi_match_list p rel eq inst any xs1 xs2 =
    match (xs1, xs2) with
    | x1::xs1, x2::xs2 ->
      (match p rel eq inst any x1 x2 with
      | Some inst -> bi_match_list p rel eq inst any xs1 xs2
      | None -> None)
    | [], [] -> Some inst
    | _, _ -> None
  in

  let update binop c t ce =
    ConEnv.add c (binop ?src_fields:None t (ConEnv.find c ce)) ce
  in

  let rec bi_match_typ rel eq ((l, u) as inst) any t1 t2 =
    if t1 == t2 || SS.mem (t1, t2) !rel
    then Some inst
    else begin
    rel := SS.add (t1, t2) !rel;
    match t1, t2 with
    | Pre, _ | _, Pre ->
      Some inst (* TODO: assert false? *)
    | Any, Any ->
      Some inst
    | _, Any when rel != eq ->
      Some inst
    | Non, Non ->
      Some inst
    | Non, _ when rel != eq ->
      Some inst
    | Named (_n, t1'), t2 ->
      bi_match_typ rel eq inst any t1' t2
    | t1, Named (_n, t2') ->
      bi_match_typ rel eq inst any t1 t2'
    | _, Con (con2, ts2) when flexible con2 ->
      assert (ts2 = []);
      if mentions t1 any || not (denotable t1) then
        None
      else Some
       (update lub con2 t1 l,
        if rel != eq then u else update glb con2 t1 u)
    | Con (con1, ts1), _ when flexible con1 ->
      assert (ts1 = []);
      if mentions t2 any || not (denotable t2) then
        None
      else Some
        ((if rel != eq then l else update lub con1 t2 l),
         update glb con1 t2 u)
    | Con (con1, _), Con (con2, _) when flexible con1 && flexible con2 ->
      (* Because we do matching, not unification, we never relate two flexible variables *)
      assert false
    | Con (con1, ts1), Con (con2, ts2) ->
      (match Cons.kind con1, Cons.kind con2 with
      | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | _, Def (tbs, t) -> (* TBR this may fail to terminate *)
        bi_match_typ rel eq inst any t1 (open_ ts2 t)
      | _ when Cons.eq con1 con2 ->
        assert (ts1 = []);
        assert (ts2 = []);
        Some inst
      | Abs (tbs, t), _ when rel != eq ->
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | _ -> None
      )
    | Con (con1, ts1), t2 ->
      (match Cons.kind con1, t2 with
      | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | Abs (tbs, t), _ when rel != eq ->
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | _ -> None
      )
    | t1, Con (con2, ts2) ->
      (match Cons.kind con2 with
      | Def (tbs, t) -> (* TBR this may fail to terminate *)
        bi_match_typ rel eq inst any t1 (open_ ts2 t)
      | _ -> None
      )
    | Prim p1, Prim p2 when p1 = p2 ->
      Some inst
    | Prim p1, Prim p2 when rel != eq ->
      if p1 = Nat && p2 = Int then Some inst else None
    | Obj (s1, tfs1), Obj (s2, tfs2) ->
      if s1 = s2 then
        bi_match_fields rel eq inst any tfs1 tfs2
      else None
    | Array t1', Array t2' ->
      bi_match_typ rel eq inst any t1' t2'
    | Opt t1', Opt t2' ->
      bi_match_typ rel eq inst any t1' t2'
    | Prim Null, Opt t2' when rel != eq ->
      Some inst
    | Variant fs1, Variant fs2 ->
      bi_match_tags rel eq inst any fs1 fs2
    | Tup ts1, Tup ts2 ->
      bi_match_list bi_match_typ rel eq inst any ts1 ts2
    | Func (s1, c1, tbs1, t11, t12), Func (s2, c2, tbs2, t21, t22) ->
      if s1 = s2 && c1 = c2 then
      (match bi_match_binds rel eq inst any tbs1 tbs2 with
       | Some (inst, ts) ->
         let any' = List.fold_right
           (fun t -> ConSet.add (fst (as_con t))) ts any
         in
         (match
           bi_match_list bi_match_typ rel eq inst any' (List.map (open_ ts) t21) (List.map (open_ ts) t11)
          with
         | Some inst ->
           bi_match_list bi_match_typ rel eq inst any' (List.map (open_ ts) t12) (List.map (open_ ts) t22)
         | None -> None)
       | None -> None
      )
      else None
    | Async (s1, t11, t12), Async (s2, t21, t22) ->
      if s1 = s2 then
        (match bi_equate_typ rel eq inst any t11 t21  with
         | Some inst ->
           bi_match_typ rel eq inst any t12 t22
         | None -> None)
      else None
    | Mut t1', Mut t2' ->
      bi_equate_typ rel eq inst any t1' t2'
    | Typ c1, Typ c2 ->
      (* NB: we assume c1, c2 closed *)
      if Type.eq t1 t2 then Some inst else None
    | _, _ -> None
    end

  and bi_equate_typ rel eq inst any t1 t2 =
    bi_match_typ eq eq inst any t1 t2

  and bi_match_fields rel eq inst any tfs1 tfs2 =
    (* Assume that tfs1 and tfs2 are sorted. *)
    match tfs1, tfs2 with
    | [], [] ->
      Some inst
    | _, [] when rel != eq ->
      Some inst
    | tf1::tfs1', tf2::tfs2' ->
      (match compare_field tf1 tf2 with
      | 0 ->
        (match bi_match_typ rel eq inst any tf1.typ tf2.typ with
         | Some inst -> bi_match_fields rel eq inst any tfs1' tfs2'
         | None -> None)
      | -1 when rel != eq ->
        bi_match_fields rel eq inst any tfs1' tfs2
      | _ -> None
      )
    | _, _ -> None

  and bi_match_tags rel eq inst any tfs1 tfs2 =
    (* Assume that tfs1 and tfs2 are sorted. *)
    match tfs1, tfs2 with
    | [], [] ->
      Some inst
    | [], _  ->
      Some inst
    | tf1::tfs1', tf2::tfs2' ->
      (match compare_field tf1 tf2 with
      | 0 ->
        (match bi_match_typ rel eq inst any tf1.typ tf2.typ with
         | Some inst -> bi_match_tags rel eq inst any tfs1' tfs2'
         | None -> None)
      | +1  when rel != eq->
        bi_match_tags rel eq inst any tfs1 tfs2'
      | _ -> None
      )
    | _, _ -> None

  and bi_match_binds rel eq inst any tbs1 tbs2 =
    let ts = open_binds tbs2 in
    match bi_match_list (bi_match_bind ts) rel eq inst any tbs2 tbs1 with
    | Some inst -> Some (inst,ts)
    | None -> None

  and bi_match_bind ts rel eq inst any tb1 tb2 =
    bi_match_typ rel eq inst any (open_ ts tb1.bound) (open_ ts tb2.bound)

  in
  bi_match_list bi_match_typ

(* Solves the given constraints in the given context.
 * Unused type variables can be deferred to the next round.
 *)
let solve ctx (ts1, ts2) deferred_typs =
  if debug then begin
    print_endline "solve ctx";
    print_endline (Printf.sprintf "var_list: %s" (String.concat ", " (List.map Cons.name ctx.var_list)));
    print_endline (Printf.sprintf "bounds: %s" (string_of_bounds ctx.bounds));
    print_endline (Printf.sprintf "variances: %s" (String.concat ", " (List.map (fun (c, t) -> Printf.sprintf "%s: %s" (Cons.name c) (Variance.string_of t)) (ConEnv.bindings ctx.variances))));
    print_endline (Printf.sprintf "subs: %s" (String.concat ", " (List.map (fun (t1, t2) -> Printf.sprintf "%s <: %s" (string_of_typ t1) (string_of_typ t2)) (List.combine ts1 ts2))));
    verify_ctx ctx;
  end;
  let no_another_round = deferred_typs = [] in
  let to_defer, defer_verify = if no_another_round then (ConSet.empty, false) else
    (* Find unused type variables to defer solving to the next round.
     * Only needed if there is another round.
     * Unused type variables are always deferred to the next round since there are no constraints that would help us solve them now.
     *)
    let unused =
      let cons1 = cons_typs ts1 in
      let cons2 = cons_typs ts2 in
      let used = ConSet.union cons1 cons2
      in
      ConSet.diff ctx.var_set used
    in
    (* Defer variables that appear in the bodies of deferred funcs, we don't need to pick a bound for them now, it might restrict the solution.
    * But we must fix variables that appear in the arguments of deferred funcs, because they are needed to infer the bodies.
    *)
    let must_fix, can_defer = List.fold_left (fun (must_fix, can_defer) t ->
      match unwrap_named t with
      | Func (_, _, _, t1, t2) ->
        let must_fix = ConSet.union must_fix (cons_typs t1) in
        let can_defer = ConSet.union can_defer (cons_typs t2) in
        (must_fix, can_defer)
      | _ ->
        (ConSet.union must_fix (cons t), can_defer)
      ) (ConSet.empty, ConSet.empty) deferred_typs
    in
    let must_fix, can_defer = ConSet.inter must_fix ctx.var_set, ConSet.inter can_defer ctx.var_set in
    let can_defer = ConSet.diff can_defer must_fix in
    let to_defer = ConSet.union unused can_defer in
    if debug then begin
      print_endline (Printf.sprintf "unused : %s" (String.concat ", " (List.map Cons.name (ConSet.elements unused))));
      print_endline (Printf.sprintf "can_defer : %s" (String.concat ", " (List.map Cons.name (ConSet.elements can_defer))));
      print_endline (Printf.sprintf "must_fix : %s" (String.concat ", " (List.map Cons.name (ConSet.elements must_fix))));
      print_endline (Printf.sprintf "to_defer : %s" (String.concat ", " (List.map Cons.name (ConSet.elements to_defer))));
    end;
    to_defer, not (ConSet.is_empty can_defer)
  in
  match
    bi_match_typs ctx (ref SS.empty) (ref SS.empty) ctx.bounds ConSet.empty ts1 ts2
  with
  | Some (l, u) ->
    if debug then begin
      print_endline (Printf.sprintf "bi_match_typs : %s" (string_of_bounds (l, u)));
    end;
    let unsolved = ref ConSet.empty in
    let env = l |> ConEnv.mapi (fun c lb ->
      let ub = ConEnv.find c u in
      if eq lb ub then
        ub
      else if sub lb ub then
        if ConSet.mem c to_defer then begin
          (* Defer solving the type parameter to the next round *)
          unsolved := ConSet.add c !unsolved;
          (ConEnv.find c ctx.var_env).t
        end else
          choose_under_constrained ctx lb c ub
      else
        fail_over_constrained lb c ub)
    in
    if debug then begin
      print_endline (Printf.sprintf "env : %s" (String.concat ", " (List.map (fun (c, t) -> Printf.sprintf "%s := %s" (Cons.name c) (string_of_typ t)) (ConEnv.bindings env))));
      print_endline (Printf.sprintf "unsolved : %s" (String.concat ", " (List.map Cons.name (ConSet.elements !unsolved))));
      print_endline "";
    end;
    let var_set = !unsolved in
    let (l, u) = ctx.bounds in
    let remaining = {
      var_set;
      var_env = ConEnv.filterDom var_set ctx.var_env;
      var_list = List.filter (fun c -> ConSet.mem c var_set) ctx.var_list;
      bounds = (
        ConEnv.filterDom var_set l,
        ConEnv.filterDom var_set u);
      variances = ConEnv.filterDom var_set ctx.variances;
      to_verify = if defer_verify then (List.map (subst env) ts1, List.map (subst env) ts2) else ([], [])
    } in
    let verify_now = if defer_verify then ctx.to_verify else
      let dts1, dts2 = ctx.to_verify in
      (dts1 @ ts1, dts2 @ ts2)
    in
    if verify_inst ~ctx ~remaining env verify_now then
      env, remaining
    else begin
      let instantiation = ConEnv.bindings env
        |> List.map (fun (c, t) -> Printf.sprintf "%s := %s" (Cons.name c) (string_of_typ t))
        |> String.concat ", "
      in
      raise (Bimatch (Printf.sprintf
        "bug: inferred bad instantiation\n  <%s>\nplease report this error message and, for now, supply an explicit instantiation instead"
        instantiation))
    end
  | None ->
    let tts =
      List.filter (fun (t1, t2) -> not (sub t1 t2)) (List.combine ts1 ts2)
    in
    raise (Bimatch (Format.asprintf
      "no instantiation of %s makes%s"
      (String.concat ", " (List.map string_of_con ctx.var_list))
      (String.concat "\nand"
        (List.map (fun (t1, t2) ->
          Format.asprintf "%a" display_rel (t1, "<:", t2))
          tts))))

let bi_match_subs scope_opt tbs typ_opt =
  (* Create a fresh constructor for each type parameter.
   * These constructors are used as type variables.
   *)
  let ts = open_binds tbs in
  let cs = List.map as_con_var ts in

  (* Extract the constructor for each type variable and create a type variable environment *)
  let var_set = ConSet.of_list cs in
  let var_env = List.fold_left2 (fun acc t tb ->
    let c = as_con_var t in

    (* Check that type parameters have closed bounds *)
    let bound = open_ ts tb.bound in
    if mentions bound var_set then
      fail_open_bound c bound;

    ConEnv.add c { t; bind = tb } acc
  ) ConEnv.empty ts tbs in

  (* Initialize lower and upper bounds for type variables *)
  let l = ConSet.fold (fun c l -> ConEnv.add c Non l) var_set ConEnv.empty in
  let u = ConSet.fold (fun c u -> ConEnv.add c (bound c) u) var_set ConEnv.empty in

  (* Fix the bound of the scope type parameter, if it is there *)
  let l, u = match scope_opt, tbs with
    | Some c, {sort = Scope; _}::tbs ->
      let c0 = as_con_var (List.hd ts) in
      ConEnv.add c0 c l,
      ConEnv.add c0 c u
    | None, {sort = Scope; _}::tbs ->
      raise (Bimatch "scope instantiation required but no scope available")
    | _, _ ->
      l,
      u
  in

  (* Compute the variances using the optional return type.
   * Only necessary when the return type is not part of the sub-typing constraints.
   *)
  let variances = Variance.variances var_set
    (Option.fold ~none:Any ~some:(open_ ts) typ_opt)
  in
  let ctx = { var_set; var_env; var_list = cs; bounds = (l, u); variances; to_verify = ([], [])} in

  fun subs deferred_typs ->
    let deferred_typs = List.map (open_ ts) deferred_typs in
    let ts1 = List.map (fun (t1, _) -> open_ ts t1) subs in
    let ts2 = List.map (fun (_, t2) -> open_ ts t2) subs in
    let env, remaining = solve ctx (ts1, ts2) (List.map (open_ ts) deferred_typs) in

    let remaining = if deferred_typs <> [] then Some remaining else begin
      (* No deferred types means no another round, which implies every type variable is solved *)
      assert (is_ctx_empty remaining);
      None
    end in
    List.map (subst env) ts, remaining

let finalize ts1 ctx subs =
  (* Finalize should only be called when there are subtyping constraints to solve *)
  assert (subs <> []);
  
  (* Solve the 2nd round of sub-type problems *)
  let env, remaining = solve ctx (List.split subs) [] in

  (* The 2nd round should not leave any remaining type variables *)
  assert (is_ctx_empty remaining);

  (* create a final combined `ts` solution *)
  let ts = List.map (fun t ->
    match t with
    | Con (c, []) -> ConEnv.find_opt c env |> Option.value ~default:t
    | _ -> t
  ) ts1 in

  (* Return the final solution together with the substitution of open type variables *)
  ts, env

let fail_when_types_are_not_closed remaining typs = if is_ctx_empty remaining then () else
  let allCons = cons_typs typs in
  let openConSet = ConSet.inter remaining.var_set allCons in
  if not (ConSet.is_empty openConSet) then
    let message = Printf.sprintf "cannot infer %s" (String.concat ", " (List.map Cons.name (ConSet.elements openConSet))) in
    raise (Bimatch message)
