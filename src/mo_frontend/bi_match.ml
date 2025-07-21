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

type result = {
  ts : typ list;
  ts_partial : typ list;
  ts_partial_con : typ list;
  substitutionEnv : typ ConEnv.t;
  unused : ConSet.t;
}

module SS = Set.Make (OrdPair)

(* Types that are denotable (ranged over) by type variables *)
let denotable t =
  let t' = normalize t in
  not (is_mut t' || is_typ t')

let bound c = match Cons.kind c with
  | Abs ([], t) -> t
  | _ -> assert false

(* Check instantiation `ts` satisfies bounds `tbs` and all the pairwise sub-typing relations in `subs`;
   used to sanity check inferred instantiations *)
let verify_inst tbs subs ts =
  List.length tbs = List.length ts &&
  List.for_all2 (fun t tb -> sub t (open_ ts tb.bound)) ts tbs &&
  List.for_all (fun (t1, t2) -> sub (open_ ts t1) (open_ ts t2)) subs

let mentions typ cons = not (ConSet.disjoint (Type.cons typ) cons)

let fail_open_bound c bd =
  let c = Cons.name c in
  raise (Bimatch (Format.asprintf
    "type parameter %s has an open bound%a\nmentioning another type parameter, so that explicit type instantiation is required due to limitation of inference"
    c (Lib.Format.display pp_typ) bd))

let as_con_var t = match as_con t with
  | c, [] -> c
  | _ -> assert false

type ctx = {
  (* Input type parameters with their bounds *)
  tbs : bind list;
  (* `con` created for each type parameter *)
  cs : con list;
  (* Type.Con(c, []) for each c in cs *)
  ts : typ list;
  (* Set of con type variables *)
  cons : ConSet.t;
  fixed : ConSet.t ref;
  can_skip_unused_under_constrained : bool ref;
  (* Variance of each con type variable *)
  variances : Variance.t ConEnv.t;
}

let choose_under_constrained ctx unused lb c ub =
  match ConEnv.find c ctx.variances with
  | Variance.Covariant -> lb
  | Variance.Contravariant -> ub
  | Variance.Bivariant -> lb
  | Variance.Invariant ->
    if ConSet.mem c !(ctx.fixed) then lb else
    if !(ctx.can_skip_unused_under_constrained) && ConSet.mem c unused then lb else
    let () = ctx.can_skip_unused_under_constrained := false in
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

let bi_match_subs scope_opt tbs typ_opt =
  let ts = open_binds tbs in
  (* print_endline "ts";
  print_endline (String.concat ", " (List.map string_of_typ ts)); *)

  let cs = List.map as_con_var ts in

  let cons = ConSet.of_list cs in

  let fixed = ref ConSet.empty in
  let can_skip_unused_under_constrained = ref true in

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
    cons;
    fixed;
    can_skip_unused_under_constrained;
    variances;
  } in

  let bi_match_list = make_bi_match_list ctx in

  fun subs ->
    (* print_endline "subs"; *)
    (* List.iter (fun (t1, t2) -> print_endline (Printf.sprintf "%s <: %s" (string_of_typ t1) (string_of_typ t2))) subs; *)

    let ts1 = List.map (fun (t1, _) -> open_ ts t1) subs in
    let ts2 = List.map (fun (_, t2) -> open_ ts t2) subs in

    (* Find unused type variables *)
    let unused = 
      let cons1 = Type.cons_typs ts1 in
      let cons2 = Type.cons_typs ts2 in
      let cons3 = Option.fold ~none:ConSet.empty ~some:Type.cons typ_opt in
      let used = ConSet.union cons1 cons2 |> ConSet.union cons3 in
      ConSet.diff cons used
    in

    (* print_endline "unused";
    print_endline (String.concat ", " (List.map Cons.name (ConSet.elements unused))); *)

    match
      bi_match_list (ref SS.empty) (ref SS.empty) (l, u) ConSet.empty ts1 ts2
    with
    | Some (l, u) ->
      (* print_endline "l";
      print_endline (String.concat ", " (List.map (fun (c, t) -> Printf.sprintf "%s: %s" (Cons.name c) (string_of_typ t)) (ConEnv.bindings l)));
      print_endline "u";
      print_endline (String.concat ", " (List.map (fun (c, t) -> Printf.sprintf "%s: %s" (Cons.name c) (string_of_typ t)) (ConEnv.bindings u))); *)

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
              choose_under_constrained ctx unused lb c ub
            else
              fail_over_constrained lb c ub)
        ctx.cs
      in
      ctx.can_skip_unused_under_constrained := false; (* only in the first round *)
      if verify_inst tbs subs us then
        let ts_partial = Lib.List.mapi2 (fun i c u ->
          if ConSet.mem c unused then Type.Var (Cons.name c, i) else u) ctx.cs us
        in
        let ts_partial_con = Lib.List.mapi2 (fun i c u ->
          if ConSet.mem (as_con_var c) unused then c else u) ctx.ts us
        in
        let substitutionEnv = List.fold_left2 (fun env c u -> if ConSet.mem c unused then env else ConEnv.add c u env) ConEnv.empty ctx.cs us in

        let fixedNow = ConEnv.keys substitutionEnv |> ConSet.of_list in
        ctx.fixed := ConSet.union !(ctx.fixed) fixedNow;
        
        { ts = us; ts_partial; ts_partial_con; substitutionEnv; unused }
      else
        raise (Bimatch
          (Printf.sprintf
             "bug: inferred bad instantiation\n  <%s>\nplease report this error message and, for now, supply an explicit instantiation instead"
            (String.concat ", " (List.map string_of_typ us))))
    | None ->
      ctx.can_skip_unused_under_constrained := false; (* only in the first round *)
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

let combine r1 r2 =
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
    ) r1.ts_partial r2.ts
