open Mo_types
open Type

open MakePretty(struct let show_stamps = false end)

(* Bi-Matching *)

module SS = Set.Make (struct type t = typ * typ let compare = compare end)

(* Types that are denotable (ranged over) by type variables *)
let denotable t =
  let t' = normalize t in
  not (is_mut t' || is_typ t')

let bound c = match Con.kind c with
  | Abs ([], t) -> t
  | _ -> assert false

(* Check instantiation `ts` satisfies bounds `tbs` and all the pairwise sub-typing relations in `subs`;
   used to sanity check inferred instantiations *)
let verify_inst tbs subs ts =
  List.length tbs = List.length ts &&
  List.for_all2 (fun t tb -> sub t (open_ ts tb.bound)) ts tbs &&
  List.for_all (fun (t1, t2) -> sub (open_ ts t1) (open_ ts t2)) subs

let bi_match_subs scope_opt tbs subs =
  let ts = open_binds tbs in

  let ts1 = List.map (fun (t1, _) -> open_ ts t1) subs in
  let ts2 = List.map (fun (_, t2) -> open_ ts t2) subs in

  let cs = List.map (fun t -> fst (as_con t)) ts in

  let flexible =
    let cons = ConSet.of_list cs in
    fun c -> ConSet.mem c cons in

  let mentions typ ce = not (ConSet.is_empty (ConSet.inter (cons typ) ce)) in

  let rec bi_match_list p rel eq inst any xs1 xs2 =
    match (xs1, xs2) with
    | x1::xs1, x2::xs2 ->
      (match p rel eq inst any x1 x2 with
      | Some inst -> bi_match_list p rel eq inst any xs1 xs2
      | None -> None)
    | [], [] -> Some inst
    | _, _ -> None
  in

  let rec bi_match_typ rel eq ((l, u) as inst) any t1 t2 =
    if t1 == t2 || SS.mem (t1, t2) !rel
    then Some inst
    else begin
    rel := SS.add (t1, t2) !rel;
    match t1, t2 with
    | Pre, _ | _, Pre ->
      Some inst
    | Any, Any ->
      Some inst
    | _, Any when rel != eq ->
      Some inst
    | Non, Non ->
      Some inst
    | Non, _ when rel != eq ->
      Some inst
    | _, Con (con2, ts2) when flexible con2 ->
      assert (ts2 = []);
      if mentions t1 any || not (denotable t1) then
        None
      else
        let l =
          match ConEnv.find_opt con2 l with
          | Some t1' ->
            let lub = lub t1 t1' in
            ConEnv.add con2 lub l
          | None -> ConEnv.add con2 t1 l
        in
        let u = if rel != eq then u else
          match ConEnv.find_opt con2 u with
          | Some t1' ->
            let glb = glb t1 t1' in
            ConEnv.add con2 glb u
          | None -> ConEnv.add con2 t1 u
        in
        Some (l, u)
    | Con (con1, ts1), _ when flexible con1 ->
      assert (ts1 = []);
      if mentions t2 any || not (denotable t2) then
        None
      else
        let l = if rel != eq then l else
          match ConEnv.find_opt con1 l with
          | Some t2' ->
            let lub = lub t2 t2' in
            ConEnv.add con1 lub l
          | None -> ConEnv.add con1 t2 l
        in
        let u =
          match ConEnv.find_opt con1 u with
          | Some t2' ->
            let glb = glb t2 t2' in
            ConEnv.add con1 glb u
          | None -> ConEnv.add con1 t2 u
        in
        Some (l, u)
    | Con (con1, ts1), Con (con2, ts2) ->
      (match Con.kind con1, Con.kind con2 with
      | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | _, Def (tbs, t) -> (* TBR this may fail to terminate *)
        bi_match_typ rel eq inst any t1 (open_ ts2 t)
      | _ when Con.eq con1 con2 ->
        assert (ts1 = []);
        assert (ts2 = []);
        Some inst
      | Abs (tbs, t), _ when rel != eq ->
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | _ -> None
      )
    | Con (con1, ts1), t2 ->
      (match Con.kind con1, t2 with
      | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | Abs (tbs, t), _ when rel != eq ->
        bi_match_typ rel eq inst any (open_ ts1 t) t2
      | _ -> None
      )
    | t1, Con (con2, ts2) ->
      (match Con.kind con2 with
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
    | Async (t11, t12), Async (t21, t22) ->
      (match bi_equate_typ rel eq inst any t11 t21  with
       | Some inst ->
         bi_match_typ rel eq inst any t12 t22
       | None -> None)
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

  and fail_under_constrained lb c ub =
    let lb = string_of_typ lb in
    let c = Con.name c in
    let ub = string_of_typ ub in
    failwith (Printf.sprintf
      "under-constrained implicit instantiation %s <: %s <: %s, with %s =/= %s;\nexplicit type instantiation required"
      lb c ub lb ub)

  and fail_over_constrained lb c ub =
    let lb = string_of_typ lb in
    let c = Con.name c in
    let ub = string_of_typ ub in
    failwith (Printf.sprintf
      "over-constrained implicit instantiation requires %s <: %s <: %s, but %s </: %s;\nno valid instantiation exists"
      lb c ub lb ub)

  and fail_open_bound c bd =
    let c = Con.name c in
    let bd = string_of_typ bd in
    failwith (Printf.sprintf
      "type parameter %s has an open bound %s mentioning another type parameter;\nexplicit type instantiation required due to limitation of inference" c bd)

  in
    let bds = List.map (fun tb -> open_ ts tb.bound) tbs in
    let ce = ConSet.of_list cs in
    List.iter2 (fun c bd -> if mentions bd ce then fail_open_bound c bd) cs bds;
    let l, u = match scope_opt, tbs with
      | Some c, {sort = Scope; _}::tbs ->
        let c0 = List.hd cs in
        ConEnv.singleton c0 c,
        ConEnv.singleton c0 c
      | None, {sort = Scope; _}::tbs ->
        failwith "scope instantiation required but no scope available"
      | _, _ ->
        ConEnv.empty,
        ConEnv.empty
    in
    match
      bi_match_list bi_match_typ
        (ref SS.empty) (ref SS.empty) (l, u) ConSet.empty ts1 ts2
    with
    | Some (l, u) ->
      let us = List.map
        (fun c ->
          match ConEnv.find_opt c l, ConEnv.find_opt c u with
          | None, None -> Non
          | None, Some ub -> glb ub (bound c)
          | Some lb, None ->
            if sub lb (bound c) then
              lb
            else fail_over_constrained lb c (bound c)
          | Some lb, Some ub ->
            let ub = glb ub (bound c) in
            if eq lb ub then
              ub
            else if sub lb ub then
              fail_under_constrained lb c ub
            else
              fail_over_constrained lb c ub)
        cs
      in
      if verify_inst tbs subs us then
        us
      else
        failwith
          (Printf.sprintf
             "bug: inferred bad instantiation\n  <%s>;\nplease report this error message as a bug and, for now, supply an explicit instantiation instead"
            (String.concat ", " (List.map string_of_typ us)))
    | None ->
      failwith (Printf.sprintf
        "no instantiation of %s makes %s"
        (String.concat ", " (List.map string_of_con cs))
        (String.concat " and "
          (List.map2 (fun t1 t2 ->
            Printf.sprintf "%s <: %s" (string_of_typ t1) (string_of_typ t2))
            ts1 ts2)))
