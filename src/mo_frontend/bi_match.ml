open Mo_types
open Type

(* TODO: consider turning off show_stamps (but then do it elsewhere too)
open MakePretty(struct let show_stamps = false end)
*)

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

type ctx = {
  (* Input type parameters with their bounds *)
  tbs : bind list;
  (* `con` created for each type parameter *)
  cs : con list;
  (* Type.Con(c, []) for each c in cs *)
  ts : typ list;
  (* Initial lower and upper bounds for type parameters *)
  bounds : typ ConEnv.t * typ ConEnv.t;
  (* Set of con type variables *)
  cons : ConSet.t;
  (* fixed : ConSet.t ref; *)
  (* can_skip_unused_under_constrained : bool ref; *)
  (* Variance of each con type variable *)
  variances : Variance.t ConEnv.t;
}

let empty_ctx = {
  tbs = [];
  cs = [];
  ts = [];
  bounds = (ConEnv.empty, ConEnv.empty);
  cons = ConSet.empty;
  variances = ConEnv.empty;
}

let ctx_add_from ctx c old_ctx =
  let { tbs; cs; ts; bounds = (l, u); cons; variances } = ctx in
  let idx = Lib.List.index_where (fun c' -> Cons.eq c c') (old_ctx.cs) |> Option.value ~default:(-1) in
  assert (idx >= 0);
  let tbs = List.nth old_ctx.tbs idx :: tbs in
  let cs = c :: cs in
  let ts = List.nth old_ctx.ts idx :: ts in
  let l = ConEnv.add c (ConEnv.find c (fst old_ctx.bounds)) l in
  let u = ConEnv.add c (ConEnv.find c (snd old_ctx.bounds)) u in
  let cons = ConSet.add c cons in
  let variances = ConEnv.add c (ConEnv.find c old_ctx.variances) variances in
  { tbs; cs; ts; bounds = (l, u); cons; variances}

type result = {
  ts : typ list;
  remaining : ctx option;
}

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
  | Con (c, []) -> ConSet.mem c ctx.cons
  | _ -> false

(* Check instantiation `ts` satisfies bounds `tbs` and all the pairwise sub-typing relations in `subs`;
   used to sanity check inferred instantiations *)
let verify_inst remaining tbs subs cs ts =
  let env = ConEnv.from_list2 cs ts in
  List.length tbs = List.length ts &&
  (* TODO: we cannot open_ as we might not have all tvars... *)
  (* List.for_all2 (fun t tb -> is_unsolved_var remaining t || sub t (open_ ts tb.bound)) ts tbs && *)
  List.for_all (fun (t1, t2) -> sub (subst env t1) (subst env t2)) subs &&
  true

let mentions typ cons = not (ConSet.disjoint (Type.cons typ) cons)

let fail_open_bound c bd =
  let c = Cons.name c in
  raise (Bimatch (Format.asprintf
    "type parameter %s has an open bound%a\nmentioning another type parameter, so that explicit type instantiation is required due to limitation of inference"
    c (Lib.Format.display pp_typ) bd))

(* let split_ctx ctx needs_2nd_round unused =
  (* If there is only one round of solving, we need to solve all the type parameters now *)
  (* If there are no unused type parameters, we are going to solve all type parameters now, remaining context is empty *)
  if not needs_2nd_round || ConSet.is_empty unused then ctx, None else
  (* Otherwise, split every field of ctx into used and unused parts *)
  let { tbs; cs; ts; bounds = (l, u); cons; variances } = ctx in
  
  let rec split_list cs xs acc1 acc2 =
    match cs, xs with
    | [], [] -> List.rev acc1, List.rev acc2
    | c::cs, x::xs ->
      if ConSet.mem c unused then
        split_list cs xs (x::acc1) acc2
      else
        split_list cs xs acc1 (x::acc2)
    | _, _ -> assert false
  in

  let tbs_unused, tbs_used = split_list cs tbs [] [] in
  let cs_unused, cs_used = List.partition (fun c -> ConSet.mem c unused) cs in
  let ts_unused, ts_used = split_list cs ts [] [] in
  let l_unused, l_used = ConEnv.partition (fun c _ -> ConSet.mem c unused) l in
  let u_unused, u_used = ConEnv.partition (fun c _ -> ConSet.mem c unused) u in
  let cons_unused, cons_used = ConSet.partition (fun c -> ConSet.mem c unused) cons in
  let variances_unused, variances_used = ConEnv.partition (fun c _ -> ConSet.mem c unused) variances in

  let ctx_used = {
    tbs = tbs_used;
    cs = cs_used;
    ts = ts_used;
    bounds = (l_used, u_used);
    cons = cons_used;
    (* fixed = fixed; *)
    (* can_skip_unused_under_constrained = can_skip_unused_under_constrained; *)
    variances = variances_used;
  } in
  let ctx_unused = {
    tbs = tbs_unused;
    cs = cs_unused;
    ts = ts_unused;
    bounds = (l_unused, u_unused);
    cons = cons_unused;
    (* fixed = fixed; *)
    (* can_skip_unused_under_constrained = can_skip_unused_under_constrained; *)
    variances = variances_unused;
  } in
  ctx_used, Some ctx_unused *)

let choose_under_constrained ctx unused lb c ub =
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

let make_bi_match_list ctx =
  (* These are the only functions that are used in the recursive calls? *)
  let flexible c = ConSet.mem c ctx.cons in
  
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

let solve (ctx : ctx) (ts1, ts2) needs_another_round =
  (* print_endline "subs"; *)
  (* List.iter (fun (t1, t2) -> print_endline (Printf.sprintf "%s <: %s" (string_of_typ t1) (string_of_typ t2))) subs; *)

  (* TODO: in the 2nd round we don't need open_, not even `subst` *)
  (* let ts1 = List.map (fun (t1, _) -> open_ ctx.ts t1) subs in
  let ts2 = List.map (fun (_, t2) -> open_ ctx.ts t2) subs in *)

  (* Find unused type variables *)
  (* TODO: dont calculate when not needs_another_round *)
  let unused = 
    let cons1 = Type.cons_typs ts1 in
    let cons2 = Type.cons_typs ts2 in
    (* let cons3 = Option.fold ~none:ConSet.empty ~some:Type.cons typ_opt in *)
    let used = ConSet.union cons1 cons2
    (* |> ConSet.union cons3 *)
    in
    ConSet.diff ctx.cons used
  in

  (* print_endline "unused";
  print_endline (String.concat ", " (List.map Cons.name (ConSet.elements unused))); *)

  let bi_match_list = make_bi_match_list ctx in
  match
    (* TODO: bounds can be empty, problem? *)
    bi_match_list (ref SS.empty) (ref SS.empty) ctx.bounds ConSet.empty ts1 ts2
  with
  | Some (l, u) ->
    (* print_endline "l";
    print_endline (String.concat ", " (List.map (fun (c, t) -> Printf.sprintf "%s: %s" (Cons.name c) (string_of_typ t)) (ConEnv.bindings l)));
    print_endline "u";
    print_endline (String.concat ", " (List.map (fun (c, t) -> Printf.sprintf "%s: %s" (Cons.name c) (string_of_typ t)) (ConEnv.bindings u))); *)

    let remaining_ctx = ref empty_ctx in
    let us = List.map
      (fun c ->
        match ConEnv.find c l, ConEnv.find c u with
        | lb, ub ->
          (* TODO: create a test that fixes an invariant variable in the 1st round and then throws on this unused variable in the 2nd round
          We need to exclude unused in the 1st round (only if there is a 2nd round!) and exclude fixed in the 2nd round *)
          (* print_endline (Printf.sprintf "c: %s, c bound: %s, lb: %s, ub: %s" (Cons.name c) (string_of_typ (bound c)) (string_of_typ lb) (string_of_typ ub)); *)
          if eq lb ub then
            ub
          else if sub lb ub then
            if needs_another_round && ConSet.mem c unused then begin
              remaining_ctx := ctx_add_from !remaining_ctx c ctx;
              Con (c, [])
            end else
              choose_under_constrained ctx unused lb c ub
          else
            fail_over_constrained lb c ub)
      ctx.cs
    in
    let remaining = !remaining_ctx in
    (* TODO: verify_inst does not work with split ctx *)
    (* TODO: in 2nd round: dont return ts : typ list, env *)
    if verify_inst remaining ctx.tbs (List.combine ts1 ts2) ctx.cs us then
      { ts = us; remaining = if remaining.cs = [] then None else Some remaining }
    else begin
      raise (Bimatch
        (Printf.sprintf
           "bug: inferred bad instantiation\n  <%s>\nplease report this error message and, for now, supply an explicit instantiation instead"
          (String.concat ", " (List.map string_of_typ us))))
    end
  | None ->
    let tts =
      List.filter (fun (t1, t2) -> not (sub t1 t2)) (List.combine ts1 ts2)
    in
    raise (Bimatch (Format.asprintf
      "no instantiation of %s makes%s"
      (String.concat ", " (List.map string_of_con ctx.cs))
      (String.concat "\nand"
        (List.map (fun (t1, t2) ->
          Format.asprintf "%a" display_rel (t1, "<:", t2))
          tts))))

let bi_match_subs scope_opt tbs typ_opt =
  let ts = open_binds tbs in
  (* print_endline "ts";
  print_endline (String.concat ", " (List.map string_of_typ ts)); *)

  let cs = List.map as_con_var ts in

  let cons = ConSet.of_list cs in

  (* Check that type parameters have closed bounds *)
  let bds = List.map (fun tb -> open_ ts tb.bound) tbs in
  List.iter2 (fun c bd -> if mentions bd cons then fail_open_bound c bd) cs bds;

  (* Initialize lower and upper bounds for type parameters *)
  let l = ConSet.fold (fun c l -> ConEnv.add c Non l) cons ConEnv.empty in
  let u = ConSet.fold (fun c u -> ConEnv.add c (bound c) u) cons ConEnv.empty in
  let l, u = match scope_opt, tbs with
    | Some c, {sort = Scope; _}::tbs ->
      let c0 = List.hd cs in
      (* let () = print_endline "scope instantiation" in
      let () = print_endline (Printf.sprintf "c: %s, c0: %s" (string_of_typ c) (Cons.name c0)) in *)
      ConEnv.add c0 c l,
      ConEnv.add c0 c u
    | None, {sort = Scope; _}::tbs ->
      raise (Bimatch "scope instantiation required but no scope available")
    | _, _ ->
      l,
      u
  in

  let variances =
    match typ_opt with
    | Some t ->
      Variance.variances cons (open_ ts t)
    | None ->
      ConSet.fold (fun c ce -> ConEnv.add c Variance.Bivariant ce) cons ConEnv.empty
  in
  let ctx = {
    tbs;
    ts;
    cs;
    bounds = (l, u);
    cons;
    variances;
  } in

  fun subs ->
    let ts1 = List.map (fun (t1, _) -> open_ ctx.ts t1) subs in
    let ts2 = List.map (fun (_, t2) -> open_ ctx.ts t2) subs in
    solve ctx (ts1, ts2)

(* let combine r1 r2 =
  (* check that the partial solutions are disjoint *)
  assert (List.for_all2 (fun t t' -> Type.is_var t || Type.is_var t') r1.ts_partial r2.ts_partial);
  (* assert (ConEnv.Dom.disjoint (ConEnv.dom r1.substitutionEnv) (ConEnv.dom r2.substitutionEnv)); *)
  (* let env = ConEnv.disjoint_union r1.substitutionEnv r2.substitutionEnv in *)

  (* combine the solutions *)
  List.map2 (fun t1 t2 ->
    match t1 with
    | Type.Var _ ->
    (* | Type.Con (c, []) when ConEnv.mem c r1.substitutionEnv-> *)
      (* non-partial has a solution for all type variables *)
      assert (not (Type.is_var t2));
      t2
    | _ ->
      (* variable fixed by the first solution *)
      t1
    ) r1.ts_partial r2.ts *)

let finalize { ts = ts1; remaining = ctx } subs =
  let ctx = Option.value ~default:empty_ctx ctx in
  
  (* Solve the 2nd round of sub-type problems *)
  let { ts = ts2; remaining } = solve ctx (List.split subs) false in

  (* The 2nd round should not leave any remaining type variables *)
  assert (remaining = None);

  (* create a substitution of remaining/open type variables *)
  let env = ConEnv.from_list2 ctx.cs ts2 in

  (* create a final combined `ts` solution *)
  let ts = List.map (fun t ->
    match t with
    | Type.Con (c, []) -> ConEnv.find_opt c env |> Option.value ~default:t
    | _ -> t
  ) ts1 in

  (* Return the final solution together with the substitution of open type variables *)
  ts, env

let fail_when_types_are_not_closed remaining typs = 
  let allCons = Type.cons_typs typs in
  let unused = Option.fold ~none:ConSet.empty ~some:(fun ctx -> ctx.cons) remaining in
  let openConSet = ConSet.inter unused allCons in
  if not (ConSet.is_empty openConSet) then begin
    let message = Printf.sprintf "cannot infer %s" (String.concat ", " (List.map Cons.name (ConSet.elements openConSet))) in
    raise (Bimatch message)
  end;
